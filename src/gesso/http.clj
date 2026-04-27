(ns gesso.http
  "Small Ring/HTMX response helpers."
  (:require
   [rum.core :as rum]))

(def html-content-type
  "text/html; charset=utf-8")

(defn html-response
  "Render a Hiccup/Rum body to a Ring HTML response."
  [body]
  {:status 200
   :headers {"content-type" html-content-type}
   :body (rum/render-static-markup body)})

(defn no-content
  "Return an empty 204 Ring response."
  []
  {:status 204
   :headers {}
   :body ""})
