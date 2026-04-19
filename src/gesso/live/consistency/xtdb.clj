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

(def default-visible-attempts
  "Default max retry count when waiting for a write to become query-visible."
  20)

(def default-visible-sleep-ms
  "Default sleep between visibility checks."
  50)

(defn wait-until-visible!
  "Poll until `visible?` returns truthy for ctx, or throw after the retry limit.

   opts:
   - :visible?   required predicate of ctx
   - :attempts   optional, defaults to 20
   - :sleep-ms   optional, defaults to 50

   Returns true when the predicate succeeds."
  [ctx {:keys [visible? attempts sleep-ms]
        :or {attempts default-visible-attempts
             sleep-ms default-visible-sleep-ms}}]
  (when-not visible?
    (throw (ex-info "Missing required visible predicate"
                    {:missing-key :visible?})))
  (loop [n 0]
    (cond
      (visible? ctx)
      true

      (>= n attempts)
      (throw (ex-info "XTDB write did not become visible before timeout"
                      {:attempts attempts
                       :sleep-ms sleep-ms}))

      :else
      (do
        (Thread/sleep sleep-ms)
        (recur (inc n))))))

(defn submit-visible-tx!
  "Submit a tx, wait until it is visible to reads, then publish the changed event.

   This is useful when the UI should not receive a live update until a follow-up
   read through `q` can already observe the new value.

   Required opts:
   - :tx         tx ops
   - :visible?   predicate of ctx that returns truthy once the write is visible
   - :changed    normalized changed payload for gesso.live.core/publish-change!

   Optional opts:
   - :event
   - :data
   - :attempts
   - :sleep-ms

   Returns a map with:
   - :tx-result
   - :visible?
   - :published"
  [ctx {:keys [tx changed event data visible? attempts sleep-ms]}]
  (when-not tx
    (throw (ex-info "Missing required tx ops"
                    {:missing-key :tx})))
  (when-not changed
    (throw (ex-info "Missing required changed payload"
                    {:missing-key :changed})))
  (let [connectable (connectable-from-ctx ctx)
        tx-result (submit-tx-raw! connectable tx)
        visible?* (wait-until-visible!
                   ctx
                   {:visible? visible?
                    :attempts attempts
                    :sleep-ms sleep-ms})
        published (live/publish-change!
                   ctx
                   {:changed changed
                    :event event
                    :data data})]
    {:tx-result tx-result
     :visible? visible?*
     :published published}))

(defn put-and-publish!
  "Put one XTDB document, wait until it is visible to reads, then publish the
   corresponding live change.

   Required opts:
   - :table       XTDB table keyword
   - :doc         document to put
   - :visible?    predicate of ctx that returns truthy once the write is visible
   - :changed     normalized changed payload for gesso.live.core/publish-change!

   Optional opts:
   - :event
   - :data
   - :attempts
   - :sleep-ms

   Returns a map with:
   - :tx-result
   - :visible?
   - :published"
  [ctx {:keys [table doc visible? changed event data attempts sleep-ms]}]
  (when-not table
    (throw (ex-info "Missing required XTDB table"
                    {:missing-key :table})))
  (when-not doc
    (throw (ex-info "Missing required XTDB document"
                    {:missing-key :doc})))
  (submit-visible-tx!
   ctx
   {:tx [[:put-docs table doc]]
    :visible? visible?
    :changed changed
    :event event
    :data data
    :attempts attempts
    :sleep-ms sleep-ms}))

(defn with-consistency
  "Provide token access to custom XTDB read logic."
  [ctx f]
  (let [consistency-token (live/current-consistency-token ctx)]
    (f {:consistency-token consistency-token})))
