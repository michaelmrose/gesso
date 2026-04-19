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
  (xt/submit-tx connectable tx))

(defn extract-consistency-token
  [await-token-source]
  (current-await-token await-token-source))

(defn q-raw
  ([connectable query]
   (xt/q connectable query))
  ([connectable query opts]
   (xt/q connectable query opts)))

(defn q
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
  20)

(def default-visible-sleep-ms
  50)

(defn wait-until-visible!
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

#_(defn submit-visible-tx!
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
(defn submit-visible-tx!
  [ctx {:keys [tx changed event data visible? attempts sleep-ms]}]
  (when-not tx
    (throw (ex-info "Missing required tx ops"
                    {:missing-key :tx})))
  (when-not changed
    (throw (ex-info "Missing required changed payload"
                    {:missing-key :changed})))
  (let [attempts'  (or attempts default-visible-attempts)
        sleep-ms'  (or sleep-ms default-visible-sleep-ms)
        connectable (connectable-from-ctx ctx)
        tx-result   (submit-tx-raw! connectable tx)
        visible?*   (wait-until-visible!
                     ctx
                     {:visible? visible?
                      :attempts attempts'
                      :sleep-ms sleep-ms'})
        published   (live/publish-change!
                     ctx
                     {:changed changed
                      :event event
                      :data data})]
    {:tx-result tx-result
     :visible? visible?*
     :published published}))

(defn put-and-publish!
  "Put one XTDB document, wait until it is visible to reads, then publish the
   corresponding live change."
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

(defn update-entity!
  "Read the current entity doc, transform it, write it back, wait for visibility,
   then publish the corresponding live change.

   Required opts:
   - :entity       vector like [:demo-counter \"global-shared-counter\"]
   - :table
   - :id
   - :query
   - :update-doc   fn of current-doc -> new-doc

   Optional opts:
   - :data
   - :event
   - :attempts
   - :sleep-ms"
  [ctx {:keys [entity table id query update-doc data event attempts sleep-ms]}]
  (when-not entity
    (throw (ex-info "Missing required entity identity"
                    {:missing-key :entity})))
  (when-not table
    (throw (ex-info "Missing required XTDB table"
                    {:missing-key :table})))
  (when-not id
    (throw (ex-info "Missing required entity id"
                    {:missing-key :id})))
  (when-not query
    (throw (ex-info "Missing required entity query"
                    {:missing-key :query})))
  (when-not update-doc
    (throw (ex-info "Missing required update-doc fn"
                    {:missing-key :update-doc})))
  (let [[entity-type entity-id] entity
        current-doc (first (q ctx query))
        new-doc (update-doc current-doc)]
    (put-and-publish!
     ctx
     {:table table
      :doc new-doc
      :visible? #(= new-doc (first (q % query)))
      :changed {:entity/type entity-type
                :entity/id entity-id
                :change/kind :updated}
      :event event
      :data data
      :attempts attempts
      :sleep-ms sleep-ms})))

(defn with-consistency
  [ctx f]
  (let [consistency-token (live/current-consistency-token ctx)]
    (f {:consistency-token consistency-token})))
