(ns gesso.components.form.attr
  "Attribute helpers for the Gesso form component.

   A Gesso form is a submission boundary around arbitrary authored content.
   This namespace only builds form-level attrs. It does not inspect children,
   fields, labels, sections, or controls."
  (:require
   [gesso.util :refer [class-names merge-attrs]]))

(def ^:private default-swap
  "innerHTML")

(def ^:private default-validation-trigger
  "validateField from:closest form delay:250ms")

(def ^:private default-validation-include
  "closest form")

(def ^:private default-validation-target
  "this")

(def ^:private default-validation-swap
  "none")

(def ^:private default-validation-sync
  "closest form:drop")

(def ^:private route-keys
  [:to :post :put :patch :delete])

(defn- present-route-keys
  [opts]
  (filterv #(some? (get opts %)) route-keys))

(defn- assert-one-route!
  [opts]
  (let [ks (present-route-keys opts)]
    (when (< 1 (count ks))
      (throw
       (ex-info
        "Gesso form expects only one submission route key."
        {:route-keys ks
         :allowed route-keys}))))
  opts)

(defn- route-entry
  [opts]
  (assert-one-route! opts)
  (cond
    (:to opts)     [:hx-post (:to opts)]
    (:post opts)   [:hx-post (:post opts)]
    (:put opts)    [:hx-put (:put opts)]
    (:patch opts)  [:hx-patch (:patch opts)]
    (:delete opts) [:hx-delete (:delete opts)]
    :else nil))

(defn submission-attrs
  "Build HTMX attrs for the form submission itself.

   Supported routing keys:
     :to      shorthand for POST
     :post
     :put
     :patch
     :delete

   Optional:
     :target
     :swap

   If no route key is present, returns an empty map. This allows a Gesso form
   to be used as a plain HTML form shell when needed."
  [{:keys [target swap] :or {swap default-swap} :as opts}]
  (if-let [[verb url] (route-entry opts)]
    (cond-> {verb url
             :hx-swap swap}
      target
      (assoc :hx-target target))
    {}))

(defn validation-sentinel-attrs
  "Build attrs for the optional hidden validation sentinel.

   The sentinel is a child element inside the form. It listens for
   `validateField` events bubbling from controls and posts the current form
   contents to the validation URL.

   The validation response is expected to use HTMX OOB swaps for field error
   containers."
  [{:keys [validate-url
           validation-trigger
           validation-include
           validation-target
           validation-swap
           validation-sync]}]
  (when validate-url
    {:data-form-validator true
     :hidden true
     :aria-hidden "true"
     :hx-post validate-url
     :hx-trigger (or validation-trigger default-validation-trigger)
     :hx-include (or validation-include default-validation-include)
     :hx-target (or validation-target default-validation-target)
     :hx-swap (or validation-swap default-validation-swap)
     :hx-sync (or validation-sync default-validation-sync)}))

(defn form-root-attrs
  "Build root attrs for a normal Gesso form.

   User attrs are merged last so callers can intentionally override low-level
   attrs when necessary."
  [class attrs props]
  (merge-attrs
   {:class (class-names "form-theme w-full" class)
    :data-form true}
   (submission-attrs props)
   attrs))

(defn inline-form-root-attrs
  "Build root attrs for a minimal inline form.

   This is used by small action helpers such as post-button. It preserves the
   real HTML form boundary and CSRF behavior without imposing normal form
   layout spacing."
  [class attrs props]
  (merge-attrs
   {:class (class-names "inline-flex m-0 p-0 border-0 bg-transparent" class)
    :data-form true
    :data-form-inline true}
   (submission-attrs props)
   attrs))

(defn submit-button-attrs
  "Build attrs for a submit button used by form helpers.

   The button type is forced to submit because helpers using this function are
   specifically submit helpers."
  [class attrs]
  (merge-attrs
   {:class (class-names "button-density" class)}
   attrs
   {:type "submit"}))
