(ns gesso.live.outbox.http-sse
  "Selected model-backed SSE transport over the shared managed HTTP ledger.
   Admission derives scope identity and authority from the compiled model,
   while physical session startup uses the existing scoped SSE primitive."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight :as application]
            [gesso.live.core :as live]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.http-route :as route]))

(defn- reject! [reason data]
  (throw (ex-info "Canonical SSE transport not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-assembled-sse
                         :reason reason))))

(defn- selected-routes! [acquisition operations]
  (let [realizations (:realizations acquisition)
        other-paths (concat (map :path (vals operations))
                            (map (comp :path :fragment-route val) realizations))]
    (when-not (seq realizations)
      (reject! :empty-stream-inventory {}))
    (reduce (fn [selected [id {:keys [stream-route fragment live-app]}]]
              (let [path (:path stream-route)
                    parts (route/route-segments path)]
                (when-not (and parts (= :get (:method stream-route))
                               (= :gesso.live.transport.sse/start-fragment-stream!
                                  (:boundary stream-route)))
                  (reject! :unsupported-stream-route {:fragment id}))
                (when (some #(route/templates-overlap? path %)
                            (concat (map :path selected) other-paths))
                  (reject! :ambiguous-stream-route {:path path}))
                (conj selected {:path path :fragment fragment :live-app live-app
                                :transport-segments parts})))
            [] (sort-by (comp pr-str key) realizations))))

(defn- authorize [assembly routes resolve-id session-authorized? request]
  (when-not (application/application-assembly? assembly)
    (reject! :stale-stream-assembly {}))
  (when-let [selected (route/matching-route routes (:uri request))]
    (when-let [context (route/selected-read-context selected request)]
      ;; The original request identifies the session to check; it is not
      ;; evidence that the session remains valid. This additional predicate
      ;; cannot replace model scope authority or supply a different principal.
      (when (or (nil? session-authorized?) (true? (session-authorized? context)))
        (let [{:keys [live-app fragment]} selected
              id (resolve-id context fragment)]
          (when (and (or (keyword? id) (integer? id) (uuid? id)
                         (and (string? id) (not (str/blank? id))))
                     (live/authorized-for-scope? live-app context
                                                 (live/fragment-scope-name live-app fragment) id))
            (select-keys (live/fragment-scope-instance live-app fragment id) [:topic :id])))))))

(defn prepare!
  "Validate stream inventory before provider startup. :resolve-id selects
   an instance, never a subscription topic or authority. The original model
   policy is evaluated at admission under the shared pending-authorization
   budget, and rechecked against the same admitted scope before each frame
   under that budget. Exhausted authorization capacity refuses the frame.
   Denied or malformed owned GETs return 403 without allocating interests.
   Optional trusted :session-authorized? receives the prepared context at
   admission and every frame check, before resolution or scope policy. It
   must return literal true based on current session validity; it cannot
   replace the principal or grant model scope authority. Without this hook,
   established checks reuse the original authenticated request and model
   policy alone. No automatic authentication renewal or frame recall.
   All admitted routes share capacity, cancellation and recovery. No claim
   of general router coverage or independently certified policy is made."
  [assembly options]
  (when-not (and (map? options) (fn? (:resolve-id options))
                 (or (not (contains? options :session-authorized?))
                     (fn? (:session-authorized? options)))
                 (every? #{:resolve-id :session-authorized? :sse-options :max-connections
                           :max-pending-authorizations} (keys options)))
    (reject! :invalid-stream-options {}))
  (when-not (application/application-assembly? assembly)
    (reject! :invalid-stream-assembly {}))
  (let [product (:operation-acquisition-assembly assembly)
        acquisition (:acquisition-assembly product)
        routes (selected-routes! acquisition (get-in product [:execution-assembly :route-assembly :routes]))
        options' (assoc (dissoc options :resolve-id :session-authorized?)
                        :authorize-subscription #(authorize assembly routes (:resolve-id options)
                                                             (:session-authorized? options) %))]
    {:wrap! #(http/wrap-authorized-sse % options' routes)}))
