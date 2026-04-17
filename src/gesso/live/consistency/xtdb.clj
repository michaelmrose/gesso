(ns gesso.live.consistency.xtdb
  "Biff-first XTDB 2 consistency adapter for gesso.live.

   Normal query/submit operations use the Biff connectable, which is usually
   :biff/conn. Await-token access uses :biff/node, because the XTDB await-token
   methods live on the XTDB node/DataSource rather than on the Hikari pool."
  (:require
   [gesso.live.core :as live]
   [xtdb.api :as xt]))

(defn connectable-from-ctx
  "Resolve the primary XTDB connectable from ctx.

   Biff-first behavior:
   - prefer :biff/conn for q/submit-tx
   - fall back to :biff/node"
  [ctx]
  (or (:biff/conn ctx)
      (:biff/node ctx)))

(defn node-from-ctx
  "Resolve the XTDB node from ctx.

   This is used for await-token access."
  [ctx]
  (:biff/node ctx))

(defn await-token-source-from-ctx
  "Resolve the object that supports getAwaitToken/setAwaitToken.

   In a Biff XTDB2 app, this should be :biff/node."
  [ctx]
  (node-from-ctx ctx))

(defn current-await-token
  "Read the current XTDB await token from the await-token source."
  [await-token-source]
  (.getAwaitToken await-token-source))

(defn apply-await-token!
  "Apply an XTDB await token to the await-token source."
  [await-token-source token]
  (when token
    (.setAwaitToken await-token-source token))
  await-token-source)

(defn submit-tx-raw!
  "Submit raw XTDB tx operations using the preferred Biff connectable."
  [connectable tx]
  (xt/submit-tx connectable tx))

(defn extract-consistency-token
  "Extract the generic consistency token from the XTDB await-token source."
  [await-token-source]
  (current-await-token await-token-source))

(defn q-raw
  "Execute a raw XTDB query."
  ([connectable query]
   (xt/q connectable query))
  ([connectable query opts]
   (xt/q connectable query opts)))

(defn q
  "Execute a live-consistent XTDB query.

   If a propagated token is present on the request, apply it to the XTDB
   await-token source before querying. Query execution itself uses the
   preferred Biff connectable."
  ([ctx query]
   (q ctx query nil))
  ([ctx query opts]
   (let [connectable (connectable-from-ctx ctx)
         await-token-source (await-token-source-from-ctx ctx)
         consistency-token (live/current-consistency-token ctx)]
     (when await-token-source
       (apply-await-token! await-token-source consistency-token))
     (if (some? opts)
       (q-raw connectable query opts)
       (q-raw connectable query)))))

(defn submit-tx!
  "Submit a tx and publish the corresponding changed event.

   Transaction submission uses the preferred Biff connectable.
   Consistency token extraction uses the XTDB node await-token source."
  [ctx {:keys [tx changed event data]}]
  (let [connectable (connectable-from-ctx ctx)
        await-token-source (await-token-source-from-ctx ctx)
        tx-result (submit-tx-raw! connectable tx)
        consistency-token (when await-token-source
                            (extract-consistency-token await-token-source))
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
