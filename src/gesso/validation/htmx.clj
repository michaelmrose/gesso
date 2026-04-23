(ns gesso.validation.htmx
  "Transforms server-side Malli explain-data into HTMX OOB swaps."
  (:require [clojure.string :as str]
            [malli.error :as me]))

(defn- path->err-id
  "Converts a Malli error path (e.g., [:user :email]) into a stable DOM ID.
   Matches the logic that will be in gesso.components.field.attr/derive-ids."
  [path]
  (let [path-str (->> path
                      (map name)
                      (str/join "-"))]
    (str path-str "-error")))

(defn render-oob-errors
  "Takes Malli explain-data and returns a Hiccup vector of OOB fragment updates.
   Each fragment targets a specific field's error container."
  [explain-data]
  (when explain-data
    (let [errors (me/humanize explain-data)]
      (into [:<>]
            (map (fn [[path msgs]]
                   (let [err-id (path->err-id [path])
                         ;; m/humanize returns vectors of strings for each path
                         msg    (first msgs)]
                     [:div {:id err-id :hx-swap-oob "innerHTML"}
                      ;; We wrap the message in a span and ensure the container is visible
                      [:span {:class "text-destructive"} msg]
                      ;; A tiny inline script to force the container to show if it was hidden
                      [:script (str "document.getElementById('" err-id "').classList.remove('hidden');"
                                    "document.getElementById('" (name path) "').setAttribute('aria-invalid', 'true');")]]))
                 errors)))))
