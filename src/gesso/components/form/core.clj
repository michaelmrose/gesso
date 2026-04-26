(ns gesso.components.form.core
  "Form submission boundary, CSRF injection, HTMX routing, optional validation
   coordination, and inline action forms."
  (:require
   [gesso.components.form.attr :as attr]
   [gesso.components.form.scripts :as scripts]
   [gesso.util :refer [merge-attrs nodes normalize-component-args split-opts]]))

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
  (when-let [token (anti-forgery-token ctx)]
    [:input {:type "hidden"
             :name "__anti-forgery-token"
             :value token}]))

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
  (let [script' (join-scripts (:_ attrs) script)]
    (cond-> attrs
      script' (assoc :_ script'))))

(defn- guard-enabled?
  [{:keys [validate-url guard?] :as props}]
  (if (contains? props :guard?)
    (boolean guard?)
    (boolean validate-url)))

(defn- maybe-add-submission-guard
  [form-attrs props]
  (if (guard-enabled? props)
    (merge-script form-attrs (scripts/submission-guard))
    form-attrs))

;; -----------------------------------------------------------------------------
;; Validation sentinel
;; -----------------------------------------------------------------------------

(defn- validation-sentinel
  [props]
  (when-let [attrs (attr/validation-sentinel-attrs props)]
    [:div attrs]))

;; -----------------------------------------------------------------------------
;; Form rendering
;; -----------------------------------------------------------------------------

(defn- form-children
  [ctx props children]
  (remove nil?
          (concat
           [(anti-forgery-input ctx)
            (validation-sentinel props)]
           children)))

(defn- build-form-attrs
  [opts]
  (let [{:keys [props class attrs]} (split-opts opts)]
    (-> (attr/form-root-attrs class attrs props)
        (maybe-add-submission-guard props))))

(defn form
  "Render a real HTML form around arbitrary authored children.

   The form component is a submission boundary. It does not inspect, generate,
   or mutate fields or controls.

   Example:
     (form ctx
       {:post \"/profile\"
        :target \"#profile-form\"
        :swap \"outerHTML\"}
       (field ...)
       [:button {:type \"submit\"} \"Save\"])

   Validation:
     Supplying :validate-url renders a hidden validation sentinel and, by
     default, attaches the submission guard.

     Use {:guard? false} to disable the guard.

   Supported route options:
     :to      shorthand for POST
     :post
     :put
     :patch
     :delete

   Other common options:
     :target
     :swap
     :validate-url
     :class
     :attrs"
  [ctx & args]
  (let [[opts children] (normalize-component-args args)
        {:keys [props]} (split-opts opts)
        form-attrs (build-form-attrs opts)]
    (into [:form form-attrs]
          (form-children ctx props children))))

(defn post-form
  "Compatibility/convenience wrapper for a POST-like form.

   Prefer `form` for authored forms. This helper exists for small generic form
   submissions and for replacing the old live.core form helpers."
  [ctx opts & children]
  (apply form ctx opts children))

;; -----------------------------------------------------------------------------
;; Inline action button
;; -----------------------------------------------------------------------------

(defn- post-button-content
  [explicit-children props]
  (cond
    (seq explicit-children)
    explicit-children

    (some? (:children props))
    (nodes (:children props))

    (some? (:label props))
    [(:label props)]

    :else
    []))

(defn- build-inline-form-attrs
  [props]
  (attr/inline-form-root-attrs
   nil
   (:form-attrs props)
   props))

(defn- build-submit-button-attrs
  [class attrs props]
  (attr/submit-button-attrs
   class
   (merge-attrs attrs (:button-attrs props))))

(defn post-button
  "Render a minimal inline form containing a single submit button.

   This is useful for standalone actions such as delete, sign out, claim,
   drop, complete, and other one-button POST/PUT/PATCH/DELETE actions.

   Example:
     (post-button ctx
       {:delete \"/sessions/current\"
        :target \"#session-panel\"
        :swap \"outerHTML\"
        :button-attrs {:class \"btn-outline\"}}
       \"Sign out\")

   Button content precedence:
     1. explicit children passed to post-button
     2. :children in opts
     3. :label in opts

   Top-level :class and :attrs apply to the button.
   Use :form-attrs to customize the inline wrapper form."
  [ctx & args]
  (let [[opts explicit-children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        form-attrs (build-inline-form-attrs props)
        button-attrs (build-submit-button-attrs class attrs props)
        content (post-button-content explicit-children props)]
    (into
     [:form form-attrs
      (anti-forgery-input ctx)]
     [(into [:button button-attrs] content)])))
