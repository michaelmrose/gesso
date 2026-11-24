(ns gesso.live.outbox.http-route
  "Shared selected HTTP route ownership and strict segment capture. Not a
   general Ring router or URL coercion system."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight.affordance :as affordance]))

(defn- segments [path]
  (str/split path #"/" -1))

(defn- parameter-segment? [segment]
  (and (str/starts-with? segment ":") (> (count segment) 1)))

(defn- concrete-segment? [segment]
  (and (re-matches #"[A-Za-z0-9._~-]+" segment)
       (not (#{"." ".."} segment))))

(defn route-segments [path]
  (when (and (string? path) (str/starts-with? path "/"))
    (let [parts (subvec (vec (segments path)) 1)
          parameters (filter parameter-segment? parts)]
      (when (and (every? #(or (concrete-segment? %)
                             (re-matches #":[A-Za-z][A-Za-z0-9_-]*" %)) parts)
                 (= (count parameters) (count (distinct parameters))))
        parts))))

(defn templates-overlap? [left right]
  ;; Same simple full-segment vocabulary as application preflight. A shared
  ;; literal or a placeholder on either side admits a common concrete path.
  (let [a (segments (first (str/split left #"[?#]" 2)))
        b (segments (first (str/split right #"[?#]" 2)))]
    (and (= (count a) (count b))
         (every? true? (map (fn [x y]
                              (or (= x y)
                                  (and (parameter-segment? x) (not (str/blank? y)))
                                  (and (parameter-segment? y) (not (str/blank? x))))) a b)))))

(defn matching-route [routes uri]
  (when (string? uri)
    (some #(when (affordance/route-template-matches? (:path %) uri) %) routes)))

(defn captured-arguments [route uri]
  ;; Match ownership first using preflight's broader matcher. Once a route
  ;; owns the request, malformed concrete values may not fall through to
  ;; trusted fallback. Only exact, uncoerced string captures qualify.
  (let [parts (subvec (vec (segments uri)) 1)
        template (:transport-segments route)]
    (when (and (= (count parts) (count template))
               (every? concrete-segment? parts))
      (into {} (keep (fn [[segment value]]
                       (when (parameter-segment? segment)
                         [(keyword (subs segment 1)) value])))
            (map vector template parts)))))
