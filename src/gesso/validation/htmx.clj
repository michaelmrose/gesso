(ns gesso.validation.htmx
  "Transforms server-side validation errors into HTMX OOB field-error swaps."
  (:require
   [clojure.string :as str]
   [malli.error :as me]))

(defn- path-segments
  [path]
  (cond
    (nil? path) []
    (sequential? path) path
    :else [path]))

(defn- segment->id-part
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x) (name x)
    (string? x) x
    :else (str x)))

(defn path->field-id
  "Convert a Malli error path into a stable field id base.

   Examples:
     [:email]       => \"email\"
     [:user :email] => \"user-email\""
  [path]
  (->> path
       path-segments
       (remove nil?)
       (map segment->id-part)
       (remove str/blank?)
       (str/join "-")))

(defn path->err-id
  "Convert a Malli error path into the matching Gesso field error id.

   Examples:
     [:email]       => \"email-error\"
     [:user :email] => \"user-email-error\""
  [path]
  (str (path->field-id path) "-error"))

(defn- message-seq?
  [x]
  (and (sequential? x)
       (every? string? x)))

(defn- flatten-humanized*
  [path x]
  (cond
    (map? x)
    (mapcat (fn [[k v]]
              (flatten-humanized* (conj path k) v))
            x)

    (message-seq? x)
    [{:path path
      :messages (vec x)}]

    (string? x)
    [{:path path
      :messages [x]}]

    :else
    []))

(defn- flatten-humanized
  [humanized]
  (flatten-humanized* [] humanized))

(defn- js-string
  [s]
  (pr-str (str s)))

(defn- reveal-error-script
  [field-id err-id]
  (str
   "(function(){"
   "var err=document.getElementById(" (js-string err-id) ");"
   "if(err){"
   "err.classList.remove('hidden');"
   "err.dataset.serverError='true';"
   "}"
   "var field=document.getElementById(" (js-string field-id) ");"
   "if(field){field.setAttribute('aria-invalid','true');}"
   "})();"))

(defn- script-node
  [js]
  [:script {:dangerouslySetInnerHTML
            {:__html js}}])

(defn- render-oob-error
  [{:keys [path messages]}]
  (let [field-id (path->field-id path)
        err-id   (path->err-id path)
        message  (first messages)]
    [:div {:id err-id
           :hx-swap-oob "innerHTML"}
     [:span {:data-validation-error-message true
             :style {:color "var(--destructive)"}}
      message]
     (script-node (reveal-error-script field-id err-id))]))

(defn render-oob-errors
  "Render Malli explain-data as HTMX out-of-band updates for field error
   containers.

   The generated ids follow the same convention as the field component:

     field id:  email
     error id:  email-error

   Nested paths are joined with hyphens:

     [:user :email] => user-email-error"
  [explain-data]
  (when explain-data
    (let [errors (-> explain-data
                     me/humanize
                     flatten-humanized)]
      (when (seq errors)
        (into [:<>]
              (map render-oob-error)
              errors)))))

(defn- normalize-error-messages
  [messages]
  (cond
    (nil? messages)
    []

    (string? messages)
    [messages]

    (sequential? messages)
    (->> messages
         (remove nil?)
         (map str)
         vec)

    :else
    [(str messages)]))

(defn- error-map-entry
  [[path messages]]
  {:path (path-segments path)
   :messages (normalize-error-messages messages)})

(defn render-oob-error-map
  "Render a friendly field error map as HTMX out-of-band updates.

   This is for app-controlled validation where the server already chose the
   user-facing messages.

   Examples:
     (render-oob-error-map
      {:username \"Username cannot start with a number.\"
       :email \"That email is already in use.\"})

     (render-oob-error-map
      {[:user :email] \"That email is already in use.\"})

   Keys may be keywords, strings, symbols, or vector paths.

   Values may be strings or a sequence of strings. When multiple messages are
   supplied, the first message is rendered."
  [errors]
  (let [entries (->> errors
                     (map error-map-entry)
                     (filter #(seq (:messages %))))]
    (when (seq entries)
      (into [:<>]
            (map render-oob-error)
            entries))))
