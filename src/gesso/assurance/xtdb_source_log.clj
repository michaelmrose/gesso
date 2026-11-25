(ns gesso.assurance.xtdb-source-log
  "Read-only, bounded observations of the installed XTDB 2.2.0-beta1
   embedded primary source log. Internal provider interface, not a receipt.
   Results contain sensitive transaction data; never publish them directly.
   No history classifier or certification authority is implied."
  (:require [xtdb.db-catalog :as db])
  (:import (java.time Instant ZonedDateTime)
           (java.util Arrays HexFormat)
           (org.apache.arrow.memory BufferAllocator)
           (xtdb TaggedValue)
           (xtdb.api.log Log Log$Record SourceMessage$Tx)
           (xtdb.api.query IKeyFn$KeyFn)
           (xtdb.arrow Relation VectorReader)
           (xtdb.database Database)
           (xtdb.util Iid MsgIdUtil)))

(def ^:private ceilings
  {:max-message-bytes (* 16 1024 1024)
   :max-rows 16384 :max-values 262144 :max-depth 64})

(def ^:private ^:dynamic *limits* nil)
(def ^:private ^:dynamic *remaining* nil)
(def ^:private ^:dynamic *depth* 0)

(defn- reject! [reason]
  (throw (ex-info "Unsupported source log observation." {::unsupported true :reason reason})))

(defn- charge! [n]
  (when (neg? (vswap! *remaining* - n))
    (reject! :decoded-value-limit)))

(defn- one-batch! [^BufferAllocator al ^bytes data f]
  (when-not (and (bytes? data) (<= (alength data) (:max-message-bytes *limits*)))
    (reject! :message-byte-limit))
  (with-open [loader (Relation/streamLoader al data)
              rel (Relation. al (.getSchema loader))]
    (when-not (.loadNextPage loader rel) (reject! :missing-arrow-batch))
    (when (> (.getRowCount rel) (:max-rows *limits*))
      (reject! :arrow-row-limit))
    (charge! (.getRowCount rel))
    (let [result (f rel)]
      (when (.loadNextPage loader rel) (reject! :multiple-arrow-batches))
      result)))

(defn- decode-value! [v]
  (charge! 1)
  (when (> *depth* (:max-depth *limits*)) (reject! :decoded-depth-limit))
  (cond
    (or (string? v) (keyword? v) (boolean? v) (integer? v) (uuid? v)) v
    (or (instance? Instant v) (instance? ZonedDateTime v)) v
    (instance? java.util.List v) (binding [*depth* (inc *depth*)] (mapv decode-value! v))
    :else (reject! :unsupported-sql-argument)))

(defn- sql-argument-rows! [al bytes]
  (if (nil? bytes)
    [[]]
    (do
      (when-not (bytes? bytes) (reject! :unsupported-argument-bytes))
      (one-batch! al bytes
                  (fn [^Relation rel]
                    (let [columns (mapv (fn [i] (str "?_" i)) (range (count (.getVectors rel))))
                          n (.getRowCount rel)]
                      (when-not (and (pos? n)
                                     (= (set columns) (set (map #(.getName ^VectorReader %) (.getVectors rel)))))
                        (reject! :unsupported-argument-relation))
                      ;; PGWire coalesces adjacent equal statements, including
                      ;; parameterless statements. Preserve every row in order.
                      (mapv (fn [i]
                              (mapv (fn [column]
                                      (decode-value! (.getObject (.vectorFor rel column) i
                                                                 IKeyFn$KeyFn/KEBAB_CASE_KEYWORD))) columns))
                            (range n))))))))

(defn- document-value! [v]
  (charge! 1)
  (when (> *depth* (:max-depth *limits*)) (reject! :decoded-depth-limit))
  (cond
    (or (map? v) (instance? java.util.Map v))
    (binding [*depth* (inc *depth*)]
      (into {} (map (fn [[k child]]
                    (when-not (keyword? k) (reject! :unsupported-document-key))
                    [k (document-value! child)])) v))
    (instance? java.util.List v) (binding [*depth* (inc *depth*)] (mapv document-value! v))
    (instance? ZonedDateTime v) v
    (instance? Instant v) v
    :else (decode-value! v)))

(defn- put-documents! [payload]
  ;; The logged table is a tagged union leg, not the caller's table alias.
  ;; Current API writes valid-time options into individual document rows;
  ;; retain outer bounds too if present instead of dropping them.
  (let [{:keys [iids documents]} payload]
    (when-not (and (every? #{:iids :documents :xt/valid-from :xt/valid-to} (keys payload))
                   (instance? TaggedValue documents) (instance? java.util.List iids))
      (reject! :unsupported-put-docs-payload))
    (let [table (.getTag ^TaggedValue documents)
          raw (.getValue ^TaggedValue documents)]
      (when-not (and (qualified-keyword? table) (instance? java.util.List raw)
                     (seq raw) (= (count raw) (count iids)))
        (reject! :unsupported-document-relation))
      (let [docs (mapv document-value! raw)]
        (doseq [[doc iid] (map vector docs iids)]
          (when-not (and (map? doc) (contains? doc :xt/id)
                         (bytes? iid) (= 16 (alength ^bytes iid))
                         (Arrays/equals ^bytes iid ^bytes (Iid/getAsIid (:xt/id doc))))
            (reject! :document-iid-mismatch)))
        {:kind :put-docs :table table :documents docs
         :iids (mapv #(.formatHex (HexFormat/of) ^bytes %) iids)
         :bounds (document-value! (select-keys payload [:xt/valid-from :xt/valid-to]))}))))

(defn- operation-bytes! [^Database database bytes]
  (let [al (.getAllocator database)
        groups (one-batch! al bytes
                        (fn [^Relation rel]
                          (when-not (= ["tx-ops"] (mapv #(.getName ^VectorReader %) (.getVectors rel)))
                            (reject! :unexpected-transaction-relation))
                          (mapv (fn [row]
                                  (let [^TaggedValue op (:tx-ops row)]
                                    (when-not (instance? TaggedValue op) (reject! :unexpected-operation))
                                    (case (.getTag op)
                                      :sql (let [{:keys [query args] :as payload} (.getValue op)]
                                             (when-not (and (string? query)
                                                            (every? #{:query :args} (keys payload)))
                                               (reject! :invalid-sql))
                                             (mapv (fn [row] {:kind :sql :query query :args row})
                                                   (sql-argument-rows! al args)))
                                      :put-docs [(put-documents! (.getValue op))]
                                      (reject! :unsupported-operation-kind))))
                                (.toMaps rel IKeyFn$KeyFn/KEBAB_CASE_KEYWORD))))]
    {:operations (into [] cat groups)
     :logged-operation-spans (mapv count groups)}))

(defn- observe! [node tx-id]
  (let [^Database database (db/primary-db node)
        ^Log log (.getSourceLog database)]
    (if-not (and (integer? tx-id) (<= 0 tx-id) (< tx-id Long/MAX_VALUE)
                 (= (.getEpoch log) (MsgIdUtil/msgIdToEpoch tx-id)
                    (MsgIdUtil/msgIdToEpoch (inc tx-id))))
      {:status :unsupported :reason :unsupported-log-identity}
      (let [records (into [] (take 2) (iterator-seq (.iterator (.readRecords log tx-id (inc tx-id)))))]
        (cond
          (empty? records) {:status :unavailable :reason :source-message-not-retained}
          (not= 1 (count records)) (reject! :ambiguous-source-message)
          :else
          (let [^Log$Record record (first records)
                message (.getMessage record)]
            (when-not (= tx-id (.getMsgId record)) (reject! :source-message-identity-mismatch))
            (if-not (instance? SourceMessage$Tx message)
              {:status :unsupported :reason :not-a-transaction-message}
              (merge (operation-bytes! database (.getTxOps ^SourceMessage$Tx message))
                     {:status :source-message-observed :tx-id tx-id
                      :scope :retained-primary-source-log-decoded-sql-and-document-puts
                      :provider-profile :xtdb-2.2.0-beta1/decoded-source-log-v1
                      :authority :none :certificate :not-issued}))))))))

(defn observe-source-message!
  "Read exactly one retained primary source message by epoch-qualified id.
   Returns :source-message-observed, :unavailable or :unsupported, never a
   partial operation sequence. Optional limits may only lower the fixed
   ceilings. Limits bound decoding work after provider record acquisition;
   they do not sandbox Arrow parsing or bound provider IO latency/memory.
   Only embedded nodes with a primary source log are supported. No commit,
   execution, writer identity, trusted acquisition or retention guarantee."
  ([node tx-id] (observe-source-message! node tx-id {}))
  ([node tx-id limits]
   (cond
     (not (and (map? limits)
               (every? (fn [[k v]] (and (contains? ceilings k) (integer? v)
                                        (<= 1 v (get ceilings k)))) limits)))
     {:status :unsupported :reason :invalid-observation-limits}

     (nil? (db/<-node node))
     {:status :unsupported :reason :embedded-primary-log-required}

     :else
     (let [limits (merge ceilings limits)]
       (binding [*limits* limits *remaining* (volatile! (:max-values limits)) *depth* 0]
         (try
           (observe! node tx-id)
           (catch InterruptedException e
             (.interrupt (Thread/currentThread))
             (throw e))
           (catch clojure.lang.ExceptionInfo e
             (if (::unsupported (ex-data e))
               {:status :unsupported :reason (:reason (ex-data e))}
               {:status :unavailable :reason :provider-read-failed}))
           (catch Exception _
             {:status :unavailable :reason :provider-read-failed})))))))

(def operation-bytes-profile :xtdb-2.2.0-beta1/decoded-operation-bytes-v1)

(defn observe-operation-bytes!
  "Decode supplied operation IPC bytes using the installed provider reader.
   This does not read the log or establish provenance. Use only with bounded
   independently qualified input; provider parsing is not sandboxed."
  [node bytes]
  (cond
    (not (and (bytes? bytes) (<= 1 (alength ^bytes bytes) (:max-message-bytes ceilings))))
    {:status :unsupported :reason :message-byte-limit}
    (nil? (db/<-node node))
    {:status :unsupported :reason :embedded-primary-log-required}
    :else
    (binding [*limits* ceilings *remaining* (volatile! (:max-values ceilings)) *depth* 0]
      (try
        (assoc (operation-bytes! (db/primary-db node) bytes)
               :status :operation-bytes-observed :provider-profile operation-bytes-profile
               :authority :none :certificate :not-issued)
        (catch InterruptedException e
          (.interrupt (Thread/currentThread))
          (throw e))
        (catch clojure.lang.ExceptionInfo e
          (if (::unsupported (ex-data e))
            {:status :unsupported :reason (:reason (ex-data e))}
            {:status :unavailable :reason :provider-read-failed}))
        (catch Exception _
          {:status :unavailable :reason :provider-read-failed})))))
