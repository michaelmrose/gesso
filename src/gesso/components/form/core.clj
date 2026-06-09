(ns gesso.components.form.core
  "Form submission boundary, CSRF injection, HTMX routing, optional validation
   coordination, and inline action forms."
  (:require
   [gesso.components.form.attr :as attr]
   [gesso.components.form.scripts :as scripts]
   [gesso.util :refer [merge-attrs nodes normalize-component-args split-opts]]))

(def ^:dynamic *debug?*
  false)

(defn- dbg
  [& xs]
  (when *debug?*
    (apply println "[gesso.form]" xs)))

;; -----------------------------------------------------------------------------
;; Anti-forgery
;; -----------------------------------------------------------------------------

(defn anti-forgery-token
  "Extract the anti-forgery token from a request/app context.

   Supports both the plain key and Biff's namespaced key."
  [ctx]
  (or (:anti-forgery-token ctx)
      (:biff/anti-forgery-token ctx)))

(defn anti-forgery-input
  "Render the hidden anti-forgery input when a token is present."
  [ctx]
  (let [token (anti-forgery-token ctx)]
    (dbg "anti-forgery-input"
         {:token-present? (boolean token)})
    (when token
      [:input {:type "hidden"
               :name "__anti-forgery-token"
               :value token}])))

(defn- csrf-enabled?
  [{:keys [csrf?] :as props}]
  (if (contains? props :csrf?)
    (boolean csrf?)
    (attr/csrf-default? props)))

;; -----------------------------------------------------------------------------
;; Script merging
;; -----------------------------------------------------------------------------

(defn- join-scripts
  [a b]
  (cond
    (and (seq a) (seq b))
    (str a "\n" b)

    (seq a)
    a

    (seq b)
    b

    :else
    nil))

(defn- merge-script
  [attrs script]
  (let [existing (:_ attrs)
        script'  (join-scripts existing script)]
    (dbg "merge-script"
         {:existing-script? (boolean (seq existing))
          :new-script?      (boolean (seq script))
          :merged-script?   (boolean (seq script'))})
    (cond-> attrs
      script' (assoc :_ script'))))

(defn- guard-enabled?
  [{:keys [validate-url guard?] :as props}]
  (let [enabled? (if (contains? props :guard?)
                   (boolean guard?)
                   (boolean validate-url))]
    (dbg "guard-enabled?"
         {:validate-url validate-url
          :guard?       guard?
          :enabled?     enabled?})
    enabled?))

(defn- maybe-add-submission-guard
  [form-attrs props]
  (if (guard-enabled? props)
    (do
      (dbg "adding submission guard")
      (merge-script form-attrs (scripts/submission-guard)))
    (do
      (dbg "not adding submission guard")
      form-attrs)))

;; -----------------------------------------------------------------------------
;; Validation sentinel
;; -----------------------------------------------------------------------------

(defn- validation-sentinel
  [props]
  (let [attrs (attr/validation-sentinel-attrs props)]
    (dbg "validation-sentinel"
         {:validate-url (:validate-url props)
          :sentinel?    (boolean attrs)
          :attrs        attrs})
    (when attrs
      [:div attrs])))

;; -----------------------------------------------------------------------------
;; Form rendering
;; -----------------------------------------------------------------------------

(defn- form-children
  [ctx props children]
  (let [anti-forgery (when (csrf-enabled? props)
                       (anti-forgery-input ctx))
        sentinel     (validation-sentinel props)
        all-children  (remove nil?
                              (concat
                               [anti-forgery sentinel]
                               children))]
    (dbg "form-children"
         {:anti-forgery?  (boolean anti-forgery)
          :sentinel?      (boolean sentinel)
          :authored-count (count children)
          :final-count    (count all-children)})
    all-children))

(defn- build-form-attrs
  [opts]
  (let [{:keys [props class attrs]} (split-opts opts)
        root-attrs                  (attr/form-root-attrs class attrs props)
        guarded-attrs               (maybe-add-submission-guard root-attrs props)]
    (dbg "build-form-attrs"
         {:props        props
          :class        class
          :raw-attrs    attrs
          :root-attrs   root-attrs
          :final-attrs  guarded-attrs
          :has-script?  (boolean (seq (:_ guarded-attrs)))
          :novalidate?  (boolean (:novalidate guarded-attrs))})
    guarded-attrs))

(defn form
  "Render a real HTML form around arbitrary authored children.

   The form component is a submission boundary. It does not inspect, generate,
   or mutate fields or controls.

   Route keys:
     :get
     :to       POST shorthand
     :post
     :put
     :patch
     :delete

   Common HTMX opts:
     :target
     :swap
     :trigger
     :include
     :indicator
     :sync
     :confirm
     :select
     :push-url

   Other useful opts:
     :inline?  true  Use inline form chrome.
     :csrf?    false Disable anti-forgery input. Defaults to false for GET
                      forms and true otherwise."
  [ctx & args]
  (let [[opts children] (normalize-component-args args)
        {:keys [props]} (split-opts opts)
        form-attrs      (build-form-attrs opts)
        final-children  (form-children ctx props children)]
    (dbg "render form"
         {:route        (or (:get props)
                            (:to props)
                            (:post props)
                            (:put props)
                            (:patch props)
                            (:delete props))
          :validate-url (:validate-url props)
          :target       (:target props)
          :swap         (:swap props)
          :child-count  (count final-children)})
    (into [:form form-attrs] final-children)))

(defn post-form
  "Compatibility/convenience wrapper for a POST-like form.

   Prefer `form` for authored forms. This helper exists for small generic form
   submissions and for replacing the old live.core form helpers."
  [ctx opts & children]
  (dbg "post-form" {:opts opts :child-count (count children)})
  (apply form ctx opts children))

;; -----------------------------------------------------------------------------
;; Inline action button
;; -----------------------------------------------------------------------------

(defn- post-button-content
  [explicit-children props]
  (let [content (cond
                  (seq explicit-children)
                  explicit-children

                  (some? (:children props))
                  (nodes (:children props))

                  (some? (:label props))
                  [(:label props)]

                  :else
                  [])]
    (dbg "post-button-content"
         {:explicit-children? (boolean (seq explicit-children))
          :opts-children?     (some? (:children props))
          :label?             (some? (:label props))
          :content-count      (count content)})
    content))

(defn- build-inline-form-attrs
  [props]
  (let [attrs (attr/inline-form-root-attrs
               nil
               (:form-attrs props)
               props)]
    (dbg "build-inline-form-attrs"
         {:props props
          :attrs attrs})
    attrs))

(defn- build-submit-button-attrs
  [class attrs props]
  (let [button-attrs (attr/submit-button-attrs
                      class
                      (merge-attrs attrs (:button-attrs props)))]
    (dbg "build-submit-button-attrs"
         {:class        class
          :attrs        attrs
          :button-attrs (:button-attrs props)
          :final        button-attrs})
    button-attrs))

(defn post-button
  "Render a minimal inline form containing a single submit button.

   This preserves the real HTML form boundary and CSRF behavior while avoiding
   normal form layout spacing."
  [ctx & args]
  (let [[opts explicit-children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        form-attrs                  (build-inline-form-attrs props)
        button-attrs                (build-submit-button-attrs class attrs props)
        content                     (post-button-content explicit-children props)
        button                      (into [:button button-attrs] content)
        children                    (remove nil?
                                            [(when (csrf-enabled? props)
                                               (anti-forgery-input ctx))
                                             button])]
    (dbg "render post-button"
         {:route       (or (:get props)
                           (:to props)
                           (:post props)
                           (:put props)
                           (:patch props)
                           (:delete props))
          :target      (:target props)
          :swap        (:swap props)
          :child-count (count children)})
    (into [:form form-attrs] children)))
