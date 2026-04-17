(ns gesso.live.bus
  "Node-local runtime bus for gesso.live."
  (:require
   [gesso.live.match :as match]))

(def ^:private subscribers-key ::subscribers)
(def ^:private indexes-key ::indexes)

(defn bus-from-ctx
  "Resolve the live bus from ctx."
  [ctx]
  (:gesso.live/bus ctx))

(defn empty-state
  "Construct an empty bus state."
  [matcher]
  {:matcher matcher
   subscribers-key (atom {})
   indexes-key (atom {})})

(defn memory-bus
  "Construct a simple in-memory bus."
  [matcher]
  (empty-state matcher))

(defn subscriber
  "Return a normalized subscriber map."
  [{:keys [subscriber/id subscription send! meta] :as m}]
  (when-not id
    (throw (ex-info "Missing subscriber id" {:subscriber m})))
  (when-not subscription
    (throw (ex-info "Missing subscriber subscription" {:subscriber m})))
  (when-not (fn? send!)
    (throw (ex-info "Missing subscriber send! fn" {:subscriber m})))
  (cond-> {:subscriber/id id
           :subscription subscription
           :send! send!}
    (some? meta) (assoc :meta meta)))

(defn- subscribers-atom
  [live-bus]
  (or (subscribers-key live-bus)
      (throw (ex-info "Bus missing subscribers atom" {:bus live-bus}))))

(defn- indexes-atom
  [live-bus]
  (or (indexes-key live-bus)
      (throw (ex-info "Bus missing indexes atom" {:bus live-bus}))))

(defn- matcher
  [live-bus]
  (or (:matcher live-bus)
      (throw (ex-info "Bus missing matcher" {:bus live-bus}))))

(defn subscribe!
  "Register one subscriber with the bus and index it."
  [live-bus raw-subscriber]
  (let [sub (subscriber raw-subscriber)
        sub-id (:subscriber/id sub)
        m (matcher live-bus)]
    (prn :bus/subscribe!
         :subscriber-id sub-id
         :subscription (:subscription sub))
    (swap! (subscribers-atom live-bus) assoc sub-id sub)
    (swap! (indexes-atom live-bus) match/index-subscription m sub)
    (prn :bus/subscribe!
         :subscriber-id sub-id
         :subscription (:subscription sub)
         :subscriber-count (count @(subscribers-atom live-bus))
         :indexes @(indexes-atom live-bus))
    sub-id))

(defn unsubscribe!
  "Remove one subscriber from the bus and unindex it."
  [live-bus subscriber-id]
  (let [subs* (subscribers-atom live-bus)
        idx* (indexes-atom live-bus)
        m (matcher live-bus)
        sub (get @subs* subscriber-id)]
    (prn :bus/unsubscribe!
         :subscriber-id subscriber-id
         :had-subscriber? (boolean sub))
    (when sub
      (swap! idx* match/unindex-subscription m sub)
      (swap! subs* dissoc subscriber-id)
      (prn :bus/unsubscribe!
           :subscriber-id subscriber-id
           :subscription (:subscription sub)
           :subscriber-count (count @subs*)
           :indexes @idx*))
    sub))

(defn replace-subscription!
  "Replace the subscription for an existing subscriber."
  [live-bus subscriber-id new-subscription]
  (let [subs* (subscribers-atom live-bus)
        idx* (indexes-atom live-bus)
        m (matcher live-bus)
        old-sub (get @subs* subscriber-id)]
    (prn :bus/replace-subscription!
         :subscriber-id subscriber-id
         :old-subscription (:subscription old-sub)
         :new-subscription new-subscription)
    (when old-sub
      (let [new-sub (assoc old-sub :subscription new-subscription)]
        (swap! idx* match/unindex-subscription m old-sub)
        (swap! subs* assoc subscriber-id new-sub)
        (swap! idx* match/index-subscription m new-sub)
        (prn :bus/replace-subscription!
             :subscriber-id subscriber-id
             :old-subscription (:subscription old-sub)
             :new-subscription new-subscription
             :indexes @idx*)
        new-sub))))

(defn candidate-subscriber-ids
  "Return candidate subscriber ids for a changed value."
  [live-bus ctx changed]
  (let [m (matcher live-bus)
        indexes @(indexes-atom live-bus)
        entries (match/candidate-entries m ctx changed)
        ids (match/candidate-subscriber-ids indexes entries)]
    (prn :bus/candidate-subscriber-ids
         :changed changed
         :entries entries
         :candidate-ids ids)
    ids))

(defn matching-subscriber-ids
  "Return final matching subscriber ids for a changed value."
  [live-bus ctx changed]
  (let [m (matcher live-bus)
        indexes @(indexes-atom live-bus)
        subscribers @(subscribers-atom live-bus)
        ids (match/matching-subscriber-ids
             {:matcher m
              :indexes indexes
              :subscribers subscribers
              :ctx ctx
              :changed changed})]
    (prn :bus/matching-subscriber-ids
         :changed changed
         :indexes indexes
         :subscriber-ids (keys subscribers)
         :matched-ids ids)
    ids))

(defn notify-subscribers!
  "Deliver an event to the provided subscriber ids."
  [live-bus subscriber-ids event]
  (let [subscribers @(subscribers-atom live-bus)]
    (prn :bus/notify!
         :subscriber-ids subscriber-ids
         :event event)
    (reduce
     (fn [summary subscriber-id]
       (if-let [sub (get subscribers subscriber-id)]
         (try
           (prn :bus/notify-one!
                :subscriber-id subscriber-id
                :subscription (:subscription sub))
           ((:send! sub) event)
           (-> summary
               (update :matched-subscriber-ids conj subscriber-id)
               (update :delivered-count inc))
           (catch Throwable t
             (prn :bus/notify-error!
                  :subscriber-id subscriber-id
                  :message (.getMessage t)
                  :class (class t))
             (-> summary
                 (update :matched-subscriber-ids conj subscriber-id)
                 (update :errors conj {:subscriber/id subscriber-id
                                       :error t}))))
         summary))
     {:matched-subscriber-ids #{}
      :delivered-count 0
      :errors []}
     subscriber-ids)))

(defn publish!
  "Publish a normalized event into the bus."
  [live-bus event]
  (let [changed (:changed event)
        matched-ids (matching-subscriber-ids live-bus {} changed)
        summary (notify-subscribers! live-bus matched-ids event)]
    (prn :bus/publish!
         :event event
         :changed changed
         :matched-ids matched-ids
         :summary summary)
    summary))

(defn subscribers-snapshot
  "Return a stable snapshot of current subscribers."
  [live-bus]
  @(subscribers-atom live-bus))

(defn indexes-snapshot
  "Return a stable snapshot of current indexes."
  [live-bus]
  @(indexes-atom live-bus))
