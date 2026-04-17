(ns gesso.live.consistency.xtdb
  "Biff-first XTDB 2 consistency adapter for gesso.live."
  (:require
   [gesso.live.core :as live]
   [xtdb.api :as xt]))

(defn connectable-from-ctx
  "Resolve the primary XTDB connectable from ctx.

   Biff-first behavior:
   - prefer :biff/conn
   - fall back to :biff/node"
  [ctx]
  (or (:biff/conn ctx)
      (:biff/node ctx)))

(defn node-from-ctx
  "Resolve the XTDB node from ctx.

   Kept for cases where caller specifically wants the node."
  [ctx]
  (:biff/node ctx))

(defn current-await-token
  "Read the current XTDB await token from the connectable."
  [connectable]
  (.getAwaitToken connectable))

(defn apply-await-token!
  "Apply an XTDB await token to the connectable."
  [connectable token]
  (when token
    (.setAwaitToken connectable token))
  connectable)

(defn submit-tx-raw!
  "Submit raw XTDB tx operations using the preferred Biff connectable."
  [connectable tx]
  (xt/submit-tx connectable tx))

(defn extract-consistency-token
  "Extract the generic consistency token from the XTDB connectable.

   XTDB stores the await token on the DataSource/connectable."
  [connectable]
  (current-await-token connectable))

(defn q-raw
  "Execute a raw XTDB query."
  ([connectable query]
   (xt/q connectable query))
  ([connectable query opts]
   (xt/q connectable query opts)))

(defn q
  "Execute a live-consistent XTDB query.

   If a propagated token is present on the request, apply it to the XTDB
   connectable before querying."
  ([ctx query]
   (q ctx query nil))
  ([ctx query opts]
   (let [connectable (connectable-from-ctx ctx)
         consistency-token (live/current-consistency-token ctx)]
     (apply-await-token! connectable consistency-token)
     (if (some? opts)
       (q-raw connectable query opts)
       (q-raw connectable query)))))

(defn submit-tx!
  "Submit a tx and publish the corresponding changed event."
  [ctx {:keys [tx changed event data]}]
  (let [connectable (connectable-from-ctx ctx)
        tx-result (submit-tx-raw! connectable tx)
        consistency-token (extract-consistency-token connectable)
        published (live/publish-change!
                   ctx
                   {:changed changed
                    :event event
                    :consistency-token consistency-token
                    :data data})]
    {:tx-result tx-result
     :consistency-token consistency-token
     :published published}))

(defn with-consistency
  "Provide token access to custom XTDB read logic."
  [ctx f]
  (let [consistency-token (live/current-consistency-token ctx)]
    (f {:consistency-token consistency-token})))
