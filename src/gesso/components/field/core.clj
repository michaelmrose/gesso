(ns gesso.components.form.core
  "The comprehensive form component.
   Handles visual hierarchy, CSRF injection, HTMX routing, and local/server validation."
  (:require [clojure.string :as str]
            [gesso.util :refer [class-names merge-attrs normalize-component-args split-opts el only-map-arg?]]
            [gesso.components.form.attr :as attr]
            [gesso.components.form.scripts :as scripts]
            [gesso.components.field.core :as field]))

(declare form)

;; -----------------------------------------------------------------------------
;; 1. Security & Network Routing
;; -----------------------------------------------------------------------------

(defn- anti-forgery-token [ctx]
  (or (:anti-forgery-token ctx)
      (:biff/anti-forgery-token ctx)))

(defn- anti-forgery-input [ctx]
  (when-let [token (anti-forgery-token ctx)]
    [:input {:type "hidden"
             :name "__anti-forgery-token"
             :value token}]))

(defn- resolve-verb-attrs
  "Translates semantic HTTP verbs into HTMX directives."
  [{:keys [post put patch delete target swap] :or {swap "innerHTML"}}]
  (let [base (cond
               post   {:hx-post post}
               put    {:hx-put put}
               patch  {:hx-patch patch}
               delete {:hx-delete delete}
               :else  {})]
    (merge base {:hx-target target :hx-swap swap})))

;; -----------------------------------------------------------------------------
;; 2. Shared Attribute Builder
;; -----------------------------------------------------------------------------

(defn- build-form-attrs
  "Calculates the merged attributes for the root form element, including
   HTMX routing, Hyperscript validation guards, and semantic theme classes."
  [opts]
  (let [{:keys [props class attrs]} (split-opts opts)
        {:keys [validate-url] :as verbs} props

        network-attrs (resolve-verb-attrs verbs)
        smart?        (boolean validate-url)
        val-attrs     (when smart? (attr/validation-attrs validate-url))
        guard-script  (when smart? (scripts/submission-guard))]

    (merge-attrs
     network-attrs
     val-attrs
     attrs
     ;; Inherit the semantic theme layout (flex-col, gap-form)
     {:class (class-names "form-theme w-full" class)}
     (when smart?
       {:_ guard-script
        :novalidate true}))))

;; -----------------------------------------------------------------------------
;; 3. Form Renderers
;; -----------------------------------------------------------------------------

(defn- form-short-form
  "Renders a completely data-driven form.
   Takes a list of field configurations and a submit button definition."
  [ctx opts]
  (let [{:keys [props]} (split-opts opts)
        {:keys [fields submit]} props
        form-attrs (build-form-attrs opts)]

    (el :form
        form-attrs
        ;; Inject CSRF, then map fields, then append the submit button
        (into [(anti-forgery-input ctx)]
              (concat
               (map #(field/field {:props %}) fields)
               ;; Inherit semantic button sizing/typography
               [[:button {:type "submit"
                          :class (class-names "button-density weight-medium-theme"
                                              (:class submit))}
                 (:text submit "Submit")]])))))

(defn- form-long-form
  "Renders the composition API form where developers explicitly pass children."
  [ctx opts children]
  (let [form-attrs (build-form-attrs opts)]
    (el :form
        form-attrs
        (into [(anti-forgery-input ctx)] children))))

;; -----------------------------------------------------------------------------
;; 4. API Gateways
;; -----------------------------------------------------------------------------

(defn form
  "Renders a visually structured, secure form. Supported call styles: short-map or long-form."
  [ctx & args]
  (if (only-map-arg? args)
    (form-short-form ctx (first args))
    (let [[opts children] (normalize-component-args args)]
      (form-long-form ctx opts children))))

(defn post-button
  "A standalone submit button that securely wraps itself in a minimal form.
   Overrides .form-theme to prevent unwanted flex/gap spacing in inline contexts."
  [ctx & args]
  (let [[opts children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        {:keys [post put patch delete target swap button-props]} props]

    (form ctx {:props {:post post
                       :put put
                       :patch patch
                       :delete delete
                       :target target
                       :swap swap}
               ;; Reset form layout so it behaves inline
               :class "inline-flex m-0 p-0 border-0 bg-transparent"}
          (into [:button (merge-attrs {:type "submit"
                                       :class (class-names "button-density" class)}
                                      attrs)]
                children))))
