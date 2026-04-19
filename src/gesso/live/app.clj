(ns gesso.live.app
  "Higher-level live app/runtime helpers.

   This namespace owns the generic plumbing that apps should not have
   to rewrite:
   - subscription token extraction
   - token parsing
   - matcher construction for simple entry-based subscriptions
   - bus construction
   - bus middleware
   - SSE handler construction"
  (:require
   [gesso.live.bus :as bus]
   [gesso.live.transport.sse :as sse]))

(defn subscription-param
  "Extract the raw subscription token from a Ring/Biff request ctx."
  [ctx]
  (or (get-in ctx [:params "subscription"])
      (get-in ctx [:params :subscription])
      (get-in ctx [:query-params "subscription"])
      (get-in ctx [:query-params :subscription])))

(defn token-map-parser
  "Build a parser from a simple token->entry map."
  [subscriptions]
  (fn [raw-subscription]
    (get subscriptions raw-subscription)))

(defn default-changed->entries
  "Default changed-event mapping.

   Expects changed payloads shaped like:
   {:entity/type ...
    :entity/id ...}"
  [_ctx changed]
  (let [entity-type (:entity/type changed)
        entity-id   (:entity/id changed)]
    (if (and entity-type entity-id)
      [[entity-type entity-id]]
      [])))

(defn entry-matcher
  "Build a matcher for the common case where a subscription resolves to a single
   entry vector like [:demo-counter \"global-shared-counter\"]."
  [{:keys [changed->entries]
    :or {changed->entries default-changed->entries}}]
  {:subscription->entries
   (fn [subscription]
     (if subscription
       [subscription]
       []))

   :changed->entries
   changed->entries})

(defn wrap-live-bus
  "Return middleware that assoc's the provided live bus into ctx."
  [live-bus]
  (fn [handler]
    (fn [ctx]
      (handler (assoc ctx :gesso.live/bus live-bus)))))

(defn sse-handler
  "Return an SSE handler backed by the provided raw-token parser."
  [{:keys [parse-subscription]}]
  (fn [ctx]
    (let [raw-subscription (subscription-param ctx)
          subscription     (parse-subscription raw-subscription)]
      (sse/handler
       {:ctx ctx
        :subscription-fn (constantly subscription)}))))

(defn subscriptions-from-configs
  "Turn a collection of feature live configs into a token->entry map.

   Each config is expected to contain:
   - :subscription/token
   - :entry"
  [configs]
  (into {}
        (map (fn [{:keys [subscription/token entry]}]
               [token entry]))
        configs))

(defn simple-system
  "Build a simple live system.

   Options:
   - :subscriptions      map of raw token -> live entry
   - :configs            collection of feature live config maps
   - :parse-subscription optional raw-token -> normalized subscription fn
   - :changed->entries   optional custom changed mapping

   Returns:
   - :live-bus
   - :middleware
   - :sse-handler
   - :subscription-param"
  [{:keys [subscriptions configs parse-subscription changed->entries]}]
  (let [subscriptions'      (or subscriptions
                                (subscriptions-from-configs configs))
        parse-subscription' (or parse-subscription
                                (token-map-parser subscriptions'))
        matcher             (entry-matcher {:changed->entries changed->entries})
        live-bus            (bus/memory-bus matcher)]
    {:live-bus live-bus
     :middleware (wrap-live-bus live-bus)
     :sse-handler (sse-handler {:parse-subscription parse-subscription'})
     :subscription-param subscription-param}))
