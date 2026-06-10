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
  [:get :to :post :put :patch :delete])

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
  "Return [semantic-method htmx-attr url].

   :to is retained as POST shorthand."
  [opts]
  (assert-one-route! opts)
  (cond
    (:get opts)    [:get :hx-get (:get opts)]
    (:to opts)     [:post :hx-post (:to opts)]
    (:post opts)   [:post :hx-post (:post opts)]
    (:put opts)    [:put :hx-put (:put opts)]
    (:patch opts)  [:patch :hx-patch (:patch opts)]
    (:delete opts) [:delete :hx-delete (:delete opts)]
    :else nil))

(defn route-method
  "Return the semantic route method for opts, or nil when the form has no
   Gesso route key."
  [opts]
  (some-> (route-entry opts) first))

(defn csrf-default?
  "Return whether a form should include the anti-forgery input by default.

   GET forms should not leak CSRF tokens into query params. Non-GET routed
   forms and plain authored forms preserve the previous default of including
   CSRF."
  [opts]
  (not= :get (route-method opts)))

(defn- optional-htmx-attrs
  [{:keys [target
           swap
           trigger
           include
           indicator
           sync
           confirm
           select
           push-url]}]
  (cond-> {}
    target
    (assoc :hx-target target)

    swap
    (assoc :hx-swap swap)

    trigger
    (assoc :hx-trigger trigger)

    include
    (assoc :hx-include include)

    indicator
    (assoc :hx-indicator indicator)

    sync
    (assoc :hx-sync sync)

    confirm
    (assoc :hx-confirm confirm)

    select
    (assoc :hx-select select)

    (some? push-url)
    (assoc :hx-push-url push-url)))

(defn submission-attrs
  "Build HTMX attrs for the form submission itself.

   Supported routing keys:
     :get
     :to      shorthand for POST
     :post
     :put
     :patch
     :delete

   Optional HTMX opts:
     :target
     :swap
     :trigger
     :include
     :indicator
     :sync
     :confirm
     :select
     :push-url

   If no route key is present, returns an empty map. This allows a Gesso form
   to be used as a plain HTML form shell when needed.

   Deliberately does not emit native :method. This preserves the original
   submission-attrs contract and keeps HTMX routing attrs separate from native
   form fallback behavior."
  [{:keys [swap] :or {swap default-swap} :as opts}]
  (if-let [[_semantic-method htmx-attr url] (route-entry opts)]
    (merge
     {htmx-attr url
      :hx-swap swap}
     (optional-htmx-attrs (assoc opts :swap swap)))
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

(defn inline-form-root-attrs
  "Build root attrs for a minimal inline form.

   This is used by small action helpers such as post-button and by authored
   inline forms that still need CSRF/HTMX handling."
  [class attrs props]
  (merge-attrs
   {:class (class-names "inline-flex m-0 p-0 border-0 bg-transparent" class)
    :data-form true
    :data-form-inline true}
   (submission-attrs props)
   attrs))

(defn normal-form-root-attrs
  [class attrs props]
  (merge-attrs
   {:class (class-names "form-theme w-full" class)
    :data-form true}
   (submission-attrs props)
   attrs))

(defn form-root-attrs
  "Build root attrs for a normal Gesso form.

   User attrs are merged last so callers can intentionally override low-level
   attrs when necessary.

   Options:
     :inline? true  Use inline form chrome instead of normal form layout."
  [class attrs props]
  (if (:inline? props)
    (inline-form-root-attrs class attrs props)
    (normal-form-root-attrs class attrs props)))

(defn submit-button-attrs
  "Build attrs for a submit button used by form helpers.

   The button type is forced to submit because helpers using this function are
   specifically submit helpers."
  [class attrs]
  (merge-attrs
   {:class (class-names "button-density" class)}
   attrs
   {:type "submit"}))
