(ns gesso.components.alert
  (:require
   [gesso.util :refer :all]
   [gesso.components.text :as text]))

(def ^:private alert-classes
  {:default "alert"
   :destructive "alert-destructive"})

(defn alert-title
  "Alert title subcomponent.

   Uses the shared text system for typography without introducing additional
   component-specific theme classes."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/text
       {:variant :label
        :as :strong
        :class class
        :attrs (merge-attrs attrs {:data-title true})
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/text
             {:variant :label
              :as :strong
              :class class
              :attrs (merge-attrs attrs {:data-title true})}
             children))))

(defn alert-content
  "Alert content subcomponent.

   Uses the shared body text system without introducing additional
   component-specific theme classes."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/text
       {:variant :body
        :as :section
        :class class
        :attrs attrs
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/text
             {:variant :body
              :as :section
              :class class
              :attrs attrs}
             children))))

(defn alert
  "Alert component.

  Short form:
    (alert {:title \"Saved\"
            :content \"Your changes were saved.\"
            :variant :default})

  Long form:
    (alert {:variant :destructive}
      (alert-title {:text \"Error\"})
      (alert-content \"Something went wrong.\"))"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title content variant]} props
          variant (or variant :default)
          cls (get alert-classes variant "alert")]
      (el :div
          {:class (class-names cls class)
           :role "alert"}
          attrs
          [(when title
             (alert-title {:text title}))
           (when content
             (if (and (sequential? content) (not (hiccup-element? content)))
               (apply alert-content {} content)
               (alert-content {:text content}))) ]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          variant (or (:variant props) :default)
          cls (get alert-classes variant "alert")]
      (el :div
          {:class (class-names cls class)
           :role "alert"}
          attrs
          children))))
