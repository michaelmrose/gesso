(ns gesso.live.consistency.xtdb
  "Biff-first XTDB 2 consistency adapter for gesso.live."
  (:require
   [gesso.live.core :as live]
   [xtdb.api :as xt]))

(defn connectable-from-ctx
  [ctx]
  (or (:biff/conn ctx)
      (:biff/node ctx)))

(defn node-from-ctx
  [ctx]
  (:biff/node ctx))

(defn await-token-source-from-ctx
  [ctx]
  (node-from-ctx ctx))

(defn current-await-token
  [await-token-source]
  (.getAwaitToken await-token-source))

(defn apply-await-token!
  [await-token-source token]
  (when token
    (.setAwaitToken await-token-source token))
  await-token-source)

(defn submit-tx-raw!
  [connectable tx]
  (let [res (xt/submit-tx connectable tx)]
    (if (instance? java.util.concurrent.CompletableFuture res)
      @res
      res)))

(defn extract-consistency-token
  [await-token-source]
  (current-await-token await-token-source))

;; Reverted: No vector unpacking hacks. XTDB 2 handles vectors perfectly.
(defn q-raw
  ([connectable query]
   (xt/q connectable query))
  ([connectable query opts]
   (if (some? opts)
     (xt/q connectable query opts)
     (xt/q connectable query))))

(defn q
  ([ctx query]
   (q ctx query nil))
  ([ctx query opts]
   (let [connectable         (connectable-from-ctx ctx)
         await-token-source  (await-token-source-from-ctx ctx)
         consistency-token   (live/current-consistency-token ctx)]
     (when await-token-source
       (apply-await-token! await-token-source consistency-token))
     (if (some? opts)
       (q-raw connectable query opts)
       (q-raw connectable query)))))

(defn submit-tx!
  [ctx {:keys [tx changed event data]}]
  (let [connectable         (connectable-from-ctx ctx)
        await-token-source  (await-token-source-from-ctx ctx)
        tx-result           (submit-tx-raw! connectable tx)
        consistency-token   (when await-token-source
                              (extract-consistency-token await-token-source))
        published           (live/publish-change!
                             ctx
                             {:changed changed
                              :event event
                              :consistency-token consistency-token
                              :data data})]
    {:tx-result tx-result
     :consistency-token consistency-token
     :published published}))

(defn submit-and-await-tx!
  [ctx {:keys [tx changed event data]}]
  (when-not tx
    (throw (ex-info "Missing required tx ops" {:missing-key :tx})))
  (when-not changed
    (throw (ex-info "Missing required changed payload" {:missing-key :changed})))

  (let [live-node           (node-from-ctx ctx)
        tx-result           (xt/execute-tx live-node tx)
        consistency-token   (extract-consistency-token live-node)
        published           (live/publish-change!
                             (dissoc ctx :biff/conn)
                             {:changed changed
                              :event event
                              :data data})]
    {:tx-result tx-result
     :consistency-token consistency-token
     :published published}))

(defn put-and-publish!
  [ctx {:keys [table doc changed event data]}]
  (when-not table
    (throw (ex-info "Missing required XTDB table" {:missing-key :table})))
  (when-not doc
    (throw (ex-info "Missing required XTDB document" {:missing-key :doc})))
  (submit-and-await-tx!
   ctx
   {:tx [[:put-docs table doc]]
    :changed changed
    :event event
    :data data}))

(defn with-consistency
  [ctx f]
  (let [consistency-token (live/current-consistency-token ctx)]
    (f {:consistency-token consistency-token})))

;; --- State Management & Ergonomics ---

(defn ->synced
  "Helper to define a standardized map for a synced live variable."
  [{:keys [table id col entity-type default] :or {default 0} :as opts}]
  {:table table
   :id id
   :col-kw col
   ;; Add entity-type for the event bus, fallback to table if not provided
   :entity-type (or entity-type table)
   :sql-str (str "SELECT * FROM " (name table) " WHERE _id = ?")
   :default default})

(defn live-read
  "Reads the current value of a synced definition from XTDB."
  [ctx {:keys [sql-str id col-kw default]}]
  (let [row (first (q ctx [sql-str id]))]
    (or (get row col-kw) default)))

(defn live-swap!
  "Reads the current state, applies f with args, writes to XTDB, and broadcasts."
  [ctx {:keys [table id col-kw entity-type] :as synced-def} f & args]
  (let [old-val (live-read ctx synced-def)
        new-val (apply f old-val args)]

    (put-and-publish!
     ctx
     {:table table
      :doc {:xt/id id col-kw new-val}
      ;; Route the broadcast using the correct entity-type
      :changed {:entity/type entity-type
                :entity/id id
                :change/kind :updated}
      :data {:reason :live-swap}})

    new-val))
