(ns gesso.live.outbox.http-fragment
  "Selected canonical authoritative fragment GET transport. The admitted
   assembly supplies the route, fragment, query, renderer and scope policy."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight :as application]
            [gesso.live.core :as live]
            [gesso.live.outbox.http-route :as route]
            [gesso.live.outbox.managed-html :as managed-html]
            [gesso.live.progression.http :as progression-http]))

(defn- reject! [reason data]
  (throw (ex-info "Canonical fragment transport not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-http-fragment
                         :reason reason))))

(defn- selected-routes! [assembly]
  (let [acquisition (:operation-acquisition-assembly assembly)
        realizations (get-in acquisition [:acquisition-assembly :realizations])
        operations (get-in acquisition [:execution-assembly :route-assembly :routes])
        other-paths (concat (map :path (vals operations))
                            (map (comp :path :stream-route val) realizations))]
    (reduce (fn [selected [id {:keys [fragment-route fragment live-app]}]]
              (let [path (:path fragment-route)
                    parts (route/route-segments path)]
                (when-not (and parts (= :get (:method fragment-route))
                               (= :gesso.live.core/render-fragment-response
                                  (:boundary fragment-route)))
                  (reject! :unsupported-fragment-route {:fragment id}))
                (when (some #(route/templates-overlap? path %)
                            (concat (map :path selected) other-paths))
                  (reject! :ambiguous-fragment-route {:path path}))
                (conj selected {:path path :fragment fragment :live-app live-app
                                :transport-segments parts})))
            [] (sort-by (comp pr-str key) realizations))))

(defn- denied [status]
  {:status status :headers {"cache-control" "no-store"
                            "content-type" "text/plain; charset=utf-8"}
   :body "Fragment request not admitted."})

(defn- valid-id? [id]
  (or (and (string? id) (not (str/blank? id)))
      (keyword? id) (integer? id) (uuid? id)))

(defn- valid-progression? [request]
  ;; Validate the wire before invoking application identity selection. The
  ;; original canonical render boundary performs the actual context binding.
  (try (progression-http/progression-from-request request) true
       (catch clojure.lang.ExceptionInfo e
         (if (= :gesso.live.progression.http/error (:error/type (ex-data e)))
           false
           (throw e)))))

(defn wrap-handler
  "Own every method on the selected fragment routes. :resolve-id is a
   trusted (fn [request fragment-name] id-or-nil) mapping from the physical
   request to a scope instance (e.g. authenticated session or URI captures).
   It selects identity, not authority. The original compiled scope policy
   still authorizes every read with progression bound before query/render.
   Use canonical checked HTML serialization rather than arbitrary response
   builders. No automatic request authentication or universal read coverage
   is implied. Other routes retain the existing trusted fallback."
  [assembly fallback options]
  (when-not (and (map? options) (= #{:resolve-id} (set (keys options)))
                 (fn? (:resolve-id options)))
    (reject! :invalid-fragment-options {}))
  (when-not (and (fn? fallback) (application/application-assembly? assembly))
    (reject! :invalid-fragment-assembly {}))
  (let [routes (selected-routes! assembly)
        resolve-id (:resolve-id options)]
    (fn [request]
      (if-let [selected (route/matching-route routes (:uri request))]
        (cond
          (not= :get (:request-method request)) (denied 405)
          (not (application/application-assembly? assembly))
          (reject! :stale-fragment-assembly {})
          :else
          (if-let [captures (route/captured-arguments selected (:uri request))]
            (let [context (-> request
                              (dissoc :params :query-params :form-params :route-params :path-params)
                              (assoc :path-params captures))]
              (if-not (valid-progression? context)
                (denied 400)
                (let [id (resolve-id context (:fragment selected))]
                  (if-not (valid-id? id)
                    (denied 400)
                    (try
                      (update (live/render-fragment-response
                               (:live-app selected) context (:fragment selected) id
                               {:response managed-html/html-response})
                              :headers assoc "cache-control" "no-store")
                      (catch clojure.lang.ExceptionInfo e
                        (if (= :gesso.live/not-authorized (:error/type (ex-data e)))
                          (denied 403)
                          (throw e))))))))
            (denied 400)))
        (fallback request)))))
