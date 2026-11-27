(ns gesso.live.outbox.http-route
  "Shared selected HTTP route ownership and strict segment capture. Not a
   general Ring router or URL coercion system."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight.affordance :as affordance]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.progression.http :as progression-http]))

(defn- segments [path]
  (str/split path #"/" -1))

(defn- parameter-segment? [segment]
  (and (str/starts-with? segment ":") (> (count segment) 1)))

(defn- concrete-segment? [segment]
  (and (re-matches #"[A-Za-z0-9._~-]+" segment)
       (not (#{"." ".."} segment))))

(defn authority-policy?
  "The declared policy names, not authentication credentials."
  [policy]
  (and (map? policy) (not (record? policy)) (nil? (meta policy))
       (= #{:deployment-id :request-gate-policy :principal-session-policy} (set (keys policy)))
       (every? #(and (keyword? %) (namespace %)) (vals policy))))

(defn authority-acceptance?
  "Explicit environmental trust selection, not authentication."
  [declaration]
  (and (map? declaration) (not (record? declaration)) (nil? (meta declaration))
       (= #{:acceptance :premise-ids} (set (keys declaration)))
       (= :trust-open-premises (:acceptance declaration))
       (set? (:premise-ids declaration)) (nil? (meta (:premise-ids declaration)))
       (= #{:reference-command-authority/request-gate :reference-command-authority/principal-session}
          (:premise-ids declaration))))

(defn authority-binding? [binding]
  (and (map? binding) (not (record? binding)) (nil? (meta binding))
       (= #{:installation-ref :policy} (set (keys binding)))
       (string? (:installation-ref binding))
       (boolean (re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
                            (:installation-ref binding)))
       (authority-policy? (:policy binding))))

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

(defn selected-read-context
  "Prepare the same captured scope and minimum-read context for selected
   fragment GET and SSE authorization. Malformed captures or progression
   carriers refuse admission before identity resolution. On a concrete
   XTDB host bind its latest completed frontier before any application
   callback, so browser progression cannot choose a historical authority
   snapshot. Each SSE reauthorization obtains its own frontier. Refuse an
   empty selected database before any callback: the absent completed frontier
   cannot establish an immutable empty snapshot. Pin the valid-time clock
   only after frontier admission. Standalone handlers without a node retain
   their provider-neutral path.
   Request progression is not proof of authority."
  [route request]
  (when-let [captures (captured-arguments route (:uri request))]
    (try
      (let [context (progression-http/bind-request-progression
                     (-> request
                         (dissoc :params :query-params :form-params :route-params :path-params)
                         (assoc :path-params captures)))]
        (if (xtdb/node-from context)
          (when-let [bound (xtdb/bind-request-frontier context nil {:require-completed? true})]
            (xtdb/bind-selected-read-clock bound))
          context))
      (catch clojure.lang.ExceptionInfo e
        (if (= :gesso.live.progression.http/error (:error/type (ex-data e)))
          nil
          (throw e))))))
