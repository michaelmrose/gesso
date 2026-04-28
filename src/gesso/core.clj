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

   [gesso.components.field.core :as field]
   [gesso.components.form.core :as form]
   [gesso.validation.core :as validation]
   [gesso.validation.htmx :as validation-htmx]

   [gesso.components.input :as input]
   [gesso.components.label :as label]
   [gesso.components.radio-group :as radio-group]
   [gesso.components.select :as select]
   [gesso.components.switch :as switch]
   [gesso.components.textarea :as textarea]
   [gesso.theme :as gtheme]
   [gesso.components.dialog.core :as dialog]
   [gesso.components.text :as text]
   [gesso.components.empty-state :as empty-state]
   [gesso.components.icon :as icon]
   [gesso.components.status-pill :as status-pill]
   [gesso.components.group :as group]
   [gesso.components.section-block :as section-block]
   [gesso.components.toolbar :as toolbar]
   [gesso.components.page :as page]
   [gesso.components.bars.core :as bars]
   [gesso.components.background.core :as background]
   [gesso.http :as http]
   [gesso.components.scroll-buffer :as scroll-buffer]
   [gesso.live.core :as live]))

;; public api

;; http
(def html-response http/html-response)
(def no-content http/no-content)

;; live
(def live-fragment live/fragment)
(def live-fragment-root-attrs live/fragment-root-attrs)
(def live-fragment-target-attrs live/fragment-target-attrs)
(def live-anti-forgery-token live/anti-forgery-token)
(def live-anti-forgery-input live/anti-forgery-input)
(def live-post-form-attrs live/post-form-attrs)
(def live-post-form live/post-form)
(def live-post-button live/post-button)
(def live-current-consistency-token live/current-consistency-token)
(def live-request? live/live-request?)
(def live-build-event live/build-event)
(def live-publish-change! live/publish-change!)

;; background
(def background background/background)

;; page
(def page page/page)
(def page-left page/page-left)
(def page-main page/page-main)
(def page-right page/page-right)
(def page-surface page/page-surface)

;; toolbar
(def toolbar toolbar/toolbar)
(def toolbar-start toolbar/toolbar-start)
(def toolbar-center toolbar/toolbar-center)
(def toolbar-end toolbar/toolbar-end)
(def toolbar-spacer toolbar/toolbar-spacer)

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

;; icons
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

;; dropdown menu
(def dropdown-menu dropdown/dropdown-menu)
(def dropdown-menu-trigger dropdown/dropdown-menu-trigger)
(def dropdown-menu-content dropdown/dropdown-menu-content)
(def dropdown-menu-item dropdown/dropdown-menu-item)
(def dropdown-menu-label dropdown/dropdown-menu-label)
(def dropdown-menu-separator dropdown/dropdown-menu-separator)
(def dropdown-menu-right-slot dropdown/dropdown-menu-right-slot)
(def dropdown-menu-indicator dropdown/dropdown-menu-indicator)

;; accordion
(def accordion accordion/accordion)
(def accordion-item accordion/accordion-item)
(def accordion-trigger accordion/accordion-trigger)
(def accordion-content accordion/accordion-content)

;; dialog
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

;; tabs
(def tabs tabs/tabs)
(def tabs-list tabs/tabs-list)
(def tabs-trigger tabs/tabs-trigger)
(def tabs-content tabs/tabs-content)

;; alert
(def alert alert/alert)
(def alert-title alert/alert-title)
(def alert-content alert/alert-content)

;; badge
(def badge badge/badge)

;; button
(def button button/button)

;; card
(def card card/card)
(def card-title card/card-title)
(def card-description card/card-description)
(def card-header card/card-header)
(def card-content card/card-content)
(def card-footer card/card-footer)

;; checkbox
(def checkbox checkbox/checkbox)

;; label
(def label label/label)

;; form controls
(def input input/input)
(def textarea textarea/textarea)
(def select select/select)
(def field field/field)

;; form
(def form form/form)
(def post-form form/post-form)
(def post-button form/post-button)
(def anti-forgery-token form/anti-forgery-token)
(def anti-forgery-input form/anti-forgery-input)

;; validation
(def field-plan validation/field-plan)
(def empty-field-plan validation/empty-field-plan)
(def render-oob-errors validation-htmx/render-oob-errors)
(def render-oob-error-map validation-htmx/render-oob-error-map)
(def path->field-id validation-htmx/path->field-id)
(def path->err-id validation-htmx/path->err-id)

;; radio
(def radio radio-group/radio)
(def radio-group radio-group/radio-group)

;; switch
(def switch switch/switch)

;; bars
(def bars bars/bars)
(def menu bars/menu)
(def menu-group bars/menu-group)
(def menu-item bars/menu-item)

;; misc
(def scroll-buffer scroll-buffer/scroll-buffer)
