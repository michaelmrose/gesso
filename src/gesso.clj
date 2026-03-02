(ns gesso
  (:require [gesso.util :as u]))

(defn- unpack
  [args]
  (let [[opts children] (u/opts+children args)
        [props class attrs] (u/split-opts opts)]
    {:opts opts
     :props props
     :class class
     :attrs attrs
     :children children}))

(defn- data-key
  [k]
  (keyword (str "data-" (name k))))

(defn- aria-key
  [k]
  (keyword (str "aria-" (name k))))

(defn- data-attrs
  [m]
  (into {}
        (comp
         (remove (comp nil? val))
         (map (fn [[k v]] [(data-key k) v])))
        m))

(defn- aria-attrs
  [m]
  (into {}
        (comp
         (remove (comp nil? val))
         (map (fn [[k v]] [(aria-key k) v])))
        m))

(defn- element*
  [tag default-class {:keys [class attrs]} children & base-attrs]
  (u/element tag
             (apply u/merge-attrs
                    {:class (u/class-names default-class class)}
                    base-attrs)
             attrs
             children))

(defn- wrapper*
  [tag {:keys [default-class props class attrs children base-attrs]}]
  (u/element tag
             (apply u/merge-attrs
                    {:class (u/class-names default-class class)}
                    base-attrs)
             attrs
             children))

(defn- tag-of
  [props default-tag]
  (or (:tag props) default-tag))

(defn- bool-str
  [x]
  (when-not (nil? x)
    (if x "true" "false")))

;; ------------------------------------------------------------
;; Primitive role / attribute wrappers
;; ------------------------------------------------------------

(defn group
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "group"}
               attrs
               children)))

(defn heading
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "heading"}
               attrs
               children)))

(defn listbox
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "listbox"}
               attrs
               children)))

(defn menu
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "menu"}
               attrs
               children)))

(defn menuitem
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")
                :role "menuitem"}
               attrs
               children)))

(defn menuitemcheckbox
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")
                :role "menuitemcheckbox"}
               attrs
               children)))

(defn menuitemradio
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")
                :role "menuitemradio"}
               attrs
               children)))

(defn option
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :div)]
    (u/element tag
               {:class class
                :role (when (not= tag :option) "option")}
               attrs
               children)))

(defn separator
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :hr)]
    (u/element tag
               {:class class
                :role "separator"}
               attrs
               children)))

(defn tablist
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "tablist"}
               attrs
               children)))

(defn tab
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")
                :role "tab"}
               attrs
               children)))

(defn tabpanel
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "tabpanel"}
               attrs
               children)))

(defn text
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :input
               {:class (u/class-names "input" class)
                :type "text"}
               attrs
               children)))

(defn search
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :input
               {:class (u/class-names "input" class)
                :type "search"}
               attrs
               children)))

(defn combobox
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :input
               {:class class
                :type (or (:type props) "text")
                :role "combobox"}
               attrs
               children)))

;; ------------------------------------------------------------
;; Alert
;; ------------------------------------------------------------

(def alert-classes
  {:default "alert"
   :destructive "alert-destructive"})

(defn alert
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        variant (or (:variant props) :default)]
    (u/element :div
               {:role "alert"
                :class (u/class-names (get alert-classes variant "alert") class)}
               attrs
               children)))

(defn alert-title
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :h5)]
    (u/element tag
               {:class class
                :data-title (or (:data-title props) true)}
               attrs
               children)))

(defn alert-content
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :section
               {:class class}
               attrs
               children)))

;; ------------------------------------------------------------
;; Badge
;; ------------------------------------------------------------

(def badge-classes
  {:default "badge"
   :primary "badge-primary"
   :secondary "badge-secondary"
   :destructive "badge-destructive"
   :outline "badge-outline"})

(defn badge
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        variant (or (:variant props) :default)
        tag (tag-of props :span)]
    (u/element tag
               {:class (u/class-names (get badge-classes variant "badge") class)}
               attrs
               children)))

;; ------------------------------------------------------------
;; Button
;; ------------------------------------------------------------

(def button-classes
  {[:default :md] "btn"
   [:default :sm] "btn-sm"
   [:default :lg] "btn-lg"
   [:default :icon] "btn-icon"
   [:default :sm-icon] "btn-sm-icon"
   [:default :lg-icon] "btn-lg-icon"

   [:primary :md] "btn-primary"
   [:primary :sm] "btn-sm-primary"
   [:primary :lg] "btn-lg-primary"
   [:primary :icon] "btn-icon-primary"
   [:primary :sm-icon] "btn-sm-icon-primary"
   [:primary :lg-icon] "btn-lg-icon-primary"

   [:secondary :md] "btn-secondary"
   [:secondary :sm] "btn-sm-secondary"
   [:secondary :lg] "btn-lg-secondary"
   [:secondary :icon] "btn-icon-secondary"
   [:secondary :sm-icon] "btn-sm-icon-secondary"
   [:secondary :lg-icon] "btn-lg-icon-secondary"

   [:outline :md] "btn-outline"
   [:outline :sm] "btn-sm-outline"
   [:outline :lg] "btn-lg-outline"
   [:outline :icon] "btn-icon-outline"
   [:outline :sm-icon] "btn-sm-icon-outline"
   [:outline :lg-icon] "btn-lg-icon-outline"

   [:ghost :md] "btn-ghost"
   [:ghost :sm] "btn-sm-ghost"
   [:ghost :lg] "btn-lg-ghost"
   [:ghost :icon] "btn-icon-ghost"
   [:ghost :sm-icon] "btn-sm-icon-ghost"
   [:ghost :lg-icon] "btn-lg-icon-ghost"

   [:link :md] "btn-link"
   [:link :sm] "btn-sm-link"
   [:link :lg] "btn-lg-link"
   [:link :icon] "btn-icon-link"
   [:link :sm-icon] "btn-sm-icon-link"
   [:link :lg-icon] "btn-lg-icon-link"

   [:destructive :md] "btn-destructive"
   [:destructive :sm] "btn-sm-destructive"
   [:destructive :lg] "btn-lg-destructive"
   [:destructive :icon] "btn-icon-destructive"
   [:destructive :sm-icon] "btn-sm-icon-destructive"
   [:destructive :lg-icon] "btn-lg-icon-destructive"})

(defn button
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :button)
        variant (or (:variant props) :default)
        size (or (:size props) :md)
        button-class (get button-classes [variant size] "btn")]
    (u/element tag
               {:class (u/class-names button-class class)
                :type (when (= tag :button) (or (:type props) "button"))}
               attrs
               children)))

(defn button-group
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :div
               (u/merge-attrs
                {:class (u/class-names "button-group" class)}
                (data-attrs {:orientation (when-let [orientation (:orientation props)]
                                            (name orientation))}))
               attrs
               children)))

;; ------------------------------------------------------------
;; Card
;; ------------------------------------------------------------

(defn card
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :div
               {:class (u/class-names "card" class)}
               attrs
               children)))

(defn card-header
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :header
               {:class class}
               attrs
               children)))

(defn card-title
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :h2)
               {:class class}
               attrs
               children)))

(defn card-description
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :p)
               {:class class}
               attrs
               children)))

(defn card-content
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :section
               {:class class}
               attrs
               children)))

(defn card-footer
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :footer
               {:class class}
               attrs
               children)))

;; ------------------------------------------------------------
;; Checkbox / Radio / Switch / Range
;; ------------------------------------------------------------

(defn checkbox
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :input
               {:class (u/class-names "input" class)
                :type "checkbox"}
               attrs
               children)))

(defn radio
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :input
               {:class (u/class-names "input" class)
                :type "radio"}
               attrs
               children)))

(defn switch
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :input
               {:class (u/class-names "input" class)
                :type "checkbox"
                :role "switch"}
               attrs
               children)))

(defn range
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :input
               {:class (u/class-names "input" class)
                :type "range"}
               attrs
               children)))

;; ------------------------------------------------------------
;; Collapsible
;; ------------------------------------------------------------

(defn collapsible
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        open? (:open props)]
    (u/element :details
               {:class class
                :open (when open? true)}
               attrs
               children)))

(defn collapsible-trigger
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :summary
               {:class class}
               attrs
               children)))

(defn collapsible-content
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :section)]
    (u/element tag
               {:class class}
               attrs
               children)))

;; ------------------------------------------------------------
;; Command
;; ------------------------------------------------------------

(defn command-dialog
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :dialog
               {:class (u/class-names "command-dialog" class)}
               attrs
               children)))

(defn command
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :div
               {:class (u/class-names "command" class)}
               attrs
               children)))

(defn command-header
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :header
               {:class class}
               attrs
               children)))

(defn command-input
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :input
               {:class class
                :type "text"}
               attrs
               children)))

;; ------------------------------------------------------------
;; Dialog
;; ------------------------------------------------------------

(defn dialog
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :dialog
               {:class (u/class-names "dialog" class)}
               attrs
               children)))

(defn dialog-content
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :div)]
    (u/element tag
               {:class class}
               attrs
               children)))

(defn dialog-header
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :header
               {:class class}
               attrs
               children)))

(defn dialog-title
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :h2)
               {:class class}
               attrs
               children)))

(defn dialog-description
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :p)
               {:class class}
               attrs
               children)))

(defn dialog-body
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :section
               {:class class}
               attrs
               children)))

(defn dialog-footer
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :footer
               {:class class}
               attrs
               children)))

;; ------------------------------------------------------------
;; Dropdown Menu
;; ------------------------------------------------------------
(defn dropdown-menu
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :div
               {:class (u/class-names "dropdown-menu" class)}
               attrs
               children)))
(defn dropdown
  [& args]
  (apply dropdown-menu args))



(defn dropdown-trigger
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")}
               attrs
               children)))

(defn dropdown-content
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :div
               (u/merge-attrs
                {:class class
                 :data-popover true}
                (data-attrs {:side (when-let [side (:side props)] (name side))
                             :align (when-let [align (:align props)] (name align))}))
               attrs
               children)))

(defn dropdown-item
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        kind (or (:kind props) :item)
        role (case kind
               :checkbox "menuitemcheckbox"
               :radio "menuitemradio"
               "menuitem")]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")
                :role role}
               attrs
               children)))

(defn dropdown-label
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "heading"}
               attrs
               children)))

(defn dropdown-separator
  [& args]
  (apply separator args))

;; ------------------------------------------------------------
;; Field / Form / Label
;; ------------------------------------------------------------

(defn form
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :form
               {:class (u/class-names "form" class)}
               attrs
               children)))

(defn fieldset
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :fieldset
               {:class (u/class-names "fieldset" class)}
               attrs
               children)))

(defn field
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :div
               (u/merge-attrs
                {:class (u/class-names "field" class)}
                (data-attrs {:orientation (when-let [o (:orientation props)] (name o))
                             :invalid (bool-str (:invalid props))}))
               attrs
               children)))

(defn label
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :label
               {:class (u/class-names "label" class)}
               attrs
               children)))

(defn field-label
  [& args]
  (apply label args))

(defn field-description
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :p)
               {:class class}
               attrs
               children)))

(defn field-error
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "alert"}
               attrs
               children)))



;; ------------------------------------------------------------
;; Input / Textarea / Select
;; ------------------------------------------------------------

(def valid-input-types
  #{"text" "email" "password" "number" "file" "tel" "url" "search"
    "date" "datetime-local" "month" "week" "time"})

(defn input
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        type (name (or (:type props) "text"))]
    (u/element :input
               {:class (u/class-names "input" class)
                :type (if (valid-input-types type) type "text")}
               attrs
               children)))

(defn textarea
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :textarea
               {:class (u/class-names "textarea" class)}
               attrs
               children)))

(defn select
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        custom? (:custom props)]
    (if custom?
      (u/element :div
                 {:class (u/class-names "select" class)}
                 attrs
                 children)
      (u/element :select
                 {:class (u/class-names "select" class)}
                 attrs
                 children))))

(defn select-trigger
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")}
               attrs
               children)))

(defn select-content
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :div
               (u/merge-attrs
                {:class class
                 :data-popover true}
                (data-attrs {:side (when-let [side (:side props)] (name side))
                             :align (when-let [align (:align props)] (name align))}))
               attrs
               children)))

(defn select-item
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class class
                :role "option"}
               attrs
               children)))

(defn select-label
  [& args]
  (apply heading args))

;; ------------------------------------------------------------
;; Kbd
;; ------------------------------------------------------------

(defn kbd
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :kbd
               {:class (u/class-names "kbd" class)}
               attrs
               children)))

;; ------------------------------------------------------------
;; Popover
;; ------------------------------------------------------------

(defn popover
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :div
               {:class (u/class-names "popover" class)}
               attrs
               children)))

(defn popover-trigger
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")}
               attrs
               children)))

(defn popover-content
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :div
               (u/merge-attrs
                {:class class
                 :data-popover true}
                (data-attrs {:side (when-let [side (:side props)] (name side))
                             :align (when-let [align (:align props)] (name align))}))
               attrs
               children)))

;; ------------------------------------------------------------
;; Sidebar
;; ------------------------------------------------------------

(defn sidebar
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :aside
               (u/merge-attrs
                {:class (u/class-names "sidebar" class)}
                (data-attrs {:side (when-let [side (:side props)] (name side))
                             :sidebar-initialized (when-not (nil? (:initialized props))
                                                    (bool-str (:initialized props)))})
                (aria-attrs {:hidden (bool-str (:hidden props))}))
               attrs
               children)))

(defn sidebar-nav
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :nav
               {:class class}
               attrs
               children)))

(defn sidebar-header
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :header
               {:class class}
               attrs
               children)))

(defn sidebar-content
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :section
               {:class class}
               attrs
               children)))

(defn sidebar-footer
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :footer
               {:class class}
               attrs
               children)))

(defn sidebar-group
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :div
               {:class class
                :role "group"}
               attrs
               children)))

(defn sidebar-heading
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :h3)
               {:class class}
               attrs
               children)))

(defn sidebar-menu
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :ul
               {:class class}
               attrs
               children)))

(defn sidebar-menu-item
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :a)]
    (u/element tag
               (u/merge-attrs
                {:class class}
                (data-attrs {:variant (when-let [v (:variant props)] (name v))
                             :size (when-let [s (:size props)] (name s))}))
               attrs
               children)))

;; ------------------------------------------------------------
;; Tables
;; ------------------------------------------------------------

(defn table
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :table
               {:class (u/class-names "table" class)}
               attrs
               children)))

(defn table-caption
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :caption
               {:class class}
               attrs
               children)))

(defn table-header
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :thead
               {:class class}
               attrs
               children)))

(defn table-body
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :tbody
               {:class class}
               attrs
               children)))

(defn table-footer
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :tfoot
               {:class class}
               attrs
               children)))

(defn table-row
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :tr
               {:class class}
               attrs
               children)))

(defn table-head
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :th
               {:class class}
               attrs
               children)))

(defn table-cell
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :td)]
    (u/element tag
               {:class class}
               attrs
               children)))

;; ------------------------------------------------------------
;; Tabs
;; ------------------------------------------------------------

(defn tabs
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :div
               {:class (u/class-names "tabs" class)}
               attrs
               children)))

(defn tabs-list
  [& args]
  (apply tablist args))

(defn tabs-trigger
  [& args]
  (apply tab args))

(defn tabs-panel
  [& args]
  (apply tabpanel args))

;; ------------------------------------------------------------
;; Toasts
;; ------------------------------------------------------------

(defn toaster
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :div
               (u/merge-attrs
                {:class (u/class-names "toaster" class)}
                (data-attrs {:align (when-let [align (:align props)] (name align))}))
               attrs
               children)))

(defn toasts
  [& args]
  (apply toaster args))



(defn toast
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :div
               (u/merge-attrs
                {:class (u/class-names "toast" class)}
                (aria-attrs {:hidden (bool-str (:hidden props))}))
               attrs
               children)))

(defn toast-content
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :div
               {:class (u/class-names "toast-content" class)}
               attrs
               children)))

(defn toast-title
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :h2)
               {:class class}
               attrs
               children)))

(defn toast-description
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :p)
               {:class class}
               attrs
               children)))

(defn toast-footer
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :footer
               {:class class}
               attrs
               children)))

(defn toast-action
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")
                :data-toast-action true}
               attrs
               children)))

(defn toast-cancel
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :button)
               {:class class
                :type (when (= (tag-of props :button) :button) "button")
                :data-toast-cancel true}
               attrs
               children)))

;; ------------------------------------------------------------
;; Tooltip
;; ------------------------------------------------------------

(defn tooltip
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        {:keys [text side align]} props]
    (u/element (tag-of props :span)
               (u/merge-attrs
                {:class class}
                {:data-tooltip text}
                (data-attrs {:side (when side (name side))
                             :align (when align (name align))}))
               attrs
               children)))

;; ------------------------------------------------------------
;; Accordion
;; ------------------------------------------------------------

(defn accordion
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               (u/merge-attrs
                {:class (u/class-names "accordion" class)}
                (data-attrs {:accordion-initialized (when-not (nil? (:initialized props))
                                                     (bool-str (:initialized props)))})
                (aria-attrs {:multiselectable (bool-str (:multiselectable props))}))
               attrs
               children)))

(defn accordion-item
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        open? (:open props)]
    (u/element :details
               {:class (u/class-names "accordion-item" class)
                :open (when open? true)}
               attrs
               children)))

(defn accordion-trigger
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :summary
               {:class (u/class-names "accordion-trigger" class)}
               attrs
               children)))

(defn accordion-content
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :section)
               {:class (u/class-names "accordion-content" class)}
               attrs
               children)))

;; ------------------------------------------------------------
;; Alert Dialog
;; ------------------------------------------------------------

(defn alert-dialog
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :dialog
               {:class (u/class-names "alert-dialog" class)}
               attrs
               children)))

(defn alert-dialog-content
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class (u/class-names "alert-dialog-content" class)}
               attrs
               children)))

(defn alert-dialog-header
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :header
               {:class (u/class-names "alert-dialog-header" class)}
               attrs
               children)))

(defn alert-dialog-title
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :h2)
               {:class (u/class-names "alert-dialog-title" class)}
               attrs
               children)))

(defn alert-dialog-description
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :p)
               {:class (u/class-names "alert-dialog-description" class)}
               attrs
               children)))

(defn alert-dialog-body
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :section
               {:class (u/class-names "alert-dialog-body" class)}
               attrs
               children)))

(defn alert-dialog-footer
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :footer
               {:class (u/class-names "alert-dialog-footer" class)}
               attrs
               children)))

(defn alert-dialog-cancel
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :button)]
    (u/element tag
               {:class (u/class-names "alert-dialog-cancel" class)
                :type (when (= tag :button) "button")
                :data-alert-dialog-cancel true}
               attrs
               children)))

(defn alert-dialog-action
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :button)]
    (u/element tag
               {:class (u/class-names "alert-dialog-action" class)
                :type (when (= tag :button) "button")
                :data-alert-dialog-action true}
               attrs
               children)))

;; ------------------------------------------------------------
;; Avatar
;; ------------------------------------------------------------

(defn avatar
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :span)
               {:class (u/class-names "avatar" class)}
               attrs
               children)))

(defn avatar-image
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :img
               {:class (u/class-names "avatar-image" class)}
               attrs
               children)))

(defn avatar-fallback
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :span)
               {:class (u/class-names "avatar-fallback" class)}
               attrs
               children)))

;; ------------------------------------------------------------
;; Breadcrumb
;; ------------------------------------------------------------

(defn breadcrumb
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :nav
               {:class (u/class-names "breadcrumb" class)
                :aria-label (or (:aria-label props) "Breadcrumb")}
               attrs
               children)))

(defn breadcrumb-list
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :ol
               {:class (u/class-names "breadcrumb-list" class)}
               attrs
               children)))

(defn breadcrumb-item
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :li
               {:class (u/class-names "breadcrumb-item" class)}
               attrs
               children)))

(defn breadcrumb-link
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :a)]
    (u/element tag
               {:class (u/class-names "breadcrumb-link" class)}
               attrs
               children)))

(defn breadcrumb-page
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :span)
               {:class (u/class-names "breadcrumb-page" class)
                :aria-current "page"}
               attrs
               children)))

(defn breadcrumb-separator
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :li)
               {:class (u/class-names "breadcrumb-separator" class)
                :role "presentation"
                :aria-hidden "true"}
               attrs
               children)))

;; ------------------------------------------------------------
;; Empty
;; ------------------------------------------------------------

(defn empty
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :section)
               {:class (u/class-names "empty" class)}
               attrs
               children)))

(defn empty-title
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :h2)
               {:class (u/class-names "empty-title" class)}
               attrs
               children)))

(defn empty-description
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :p)
               {:class (u/class-names "empty-description" class)}
               attrs
               children)))

(defn empty-actions
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class (u/class-names "empty-actions" class)}
               attrs
               children)))

;; ------------------------------------------------------------
;; Input Group
;; ------------------------------------------------------------

(defn input-group
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class (u/class-names "input-group" class)}
               attrs
               children)))

(defn input-addon
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :span)]
    (u/element tag
               {:class (u/class-names "input-addon" class)}
               attrs
               children)))

;; ------------------------------------------------------------
;; Item
;; ------------------------------------------------------------

(defn item
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :div)]
    (u/element tag
               {:class (u/class-names "item" class)}
               attrs
               children)))

(defn item-media
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class (u/class-names "item-media" class)}
               attrs
               children)))

(defn item-body
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class (u/class-names "item-body" class)}
               attrs
               children)))

(defn item-title
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :h3)
               {:class (u/class-names "item-title" class)}
               attrs
               children)))

(defn item-description
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :p)
               {:class (u/class-names "item-description" class)}
               attrs
               children)))

(defn item-actions
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class (u/class-names "item-actions" class)}
               attrs
               children)))

;; ------------------------------------------------------------
;; Pagination
;; ------------------------------------------------------------

(defn pagination
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :nav
               {:class (u/class-names "pagination" class)
                :aria-label (or (:aria-label props) "Pagination")}
               attrs
               children)))

(defn pagination-content
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :ul
               {:class (u/class-names "pagination-content" class)}
               attrs
               children)))

(defn pagination-item
  [& args]
  (let [{:keys [class attrs children]} (unpack args)]
    (u/element :li
               {:class (u/class-names "pagination-item" class)}
               attrs
               children)))

(defn pagination-link
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :a)]
    (u/element tag
               (u/merge-attrs
                {:class (u/class-names "pagination-link" class)}
                (aria-attrs {:current (when (:current props) "page")}))
               attrs
               children)))

(defn pagination-previous
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :a)]
    (u/element tag
               {:class (u/class-names "pagination-previous" class)}
               attrs
               children)))

(defn pagination-next
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :a)]
    (u/element tag
               {:class (u/class-names "pagination-next" class)}
               attrs
               children)))

(defn pagination-ellipsis
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :span)
               {:class (u/class-names "pagination-ellipsis" class)
                :aria-hidden "true"}
               attrs
               (if (seq children) children ["…"]))))

;; ------------------------------------------------------------
;; Progress
;; ------------------------------------------------------------

(defn progress
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :progress)
        value (:value props)
        max (or (:max props) 100)]
    (u/element tag
               (u/merge-attrs
                {:class (u/class-names "progress" class)}
                (when (= tag :progress)
                  {:value value
                   :max max})
                (when (not= tag :progress)
                  (aria-attrs {:valuenow (when-not (nil? value) (str value))
                               :valuemin "0"
                               :valuemax (str max)})))
               attrs
               children)))

;; ------------------------------------------------------------
;; Radio Group
;; ------------------------------------------------------------

(defn radio-group
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element (tag-of props :div)
               {:class (u/class-names "radio-group" class)
                :role "radiogroup"}
               attrs
               children)))

;; ------------------------------------------------------------
;; Skeleton
;; ------------------------------------------------------------

(defn skeleton
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :div)]
    (u/element tag
               (u/merge-attrs
                {:class (u/class-names "skeleton" class)}
                (data-attrs {:variant (when-let [v (:variant props)] (name v))}))
               attrs
               children)))

;; ------------------------------------------------------------
;; Slider
;; ------------------------------------------------------------

(defn slider
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)]
    (u/element :input
               {:class (u/class-names "slider" "input" class)
                :type "range"}
               attrs
               children)))

;; ------------------------------------------------------------
;; Spinner
;; ------------------------------------------------------------

(defn spinner
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :div)]
    (u/element tag
               (u/merge-attrs
                {:class (u/class-names "spinner" class)}
                (aria-attrs {:label (or (:label props) "Loading")}))
               attrs
               (if (seq children) children [[:span {:class "sr-only"} (or (:label props) "Loading")]]))))

;; ------------------------------------------------------------
;; Theme Switcher
;; ------------------------------------------------------------

(defn theme-switcher
  [& args]
  (let [{:keys [props class attrs children]} (unpack args)
        tag (tag-of props :button)]
    (u/element tag
               (u/merge-attrs
                {:class (u/class-names "theme-switcher" class)
                 :type (when (= tag :button) "button")}
                (data-attrs {:theme-switcher true
                             :theme (when-let [theme (:theme props)] (name theme))}))
               attrs
               children)))
