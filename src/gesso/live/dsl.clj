(ns gesso.live.dsl
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [gesso.live.consistency.xtdb :as live.xtdb]))

(defonce !registry
  (atom {}))

(defonce !ops
  (atom {}))

(def default-op-prefix
  "/app/live/op")

(defn- table->entity-type
  [table]
  (-> table
      name
      (str/replace "_" "-")
      (#(if (str/ends-with? % "s")
          (subs % 0 (dec (count %)))
          %))
      keyword))

(defn- col->query-column
  [col]
  (if-let [ns-part (namespace col)]
    (str ns-part "$" (name col))
    (name col)))

(defn- col->query-key
  [col]
  (if-let [ns-part (namespace col)]
    (keyword (str ns-part "$" (name col)))
    (keyword (name col))))

(defn operation-id
  [ns-sym op-sym]
  (str ns-sym "/" op-sym))

(defmacro op-path
  [op-sym]
  (str default-op-prefix "/" (ns-name *ns*) "/" (name op-sym)))

(defn operation-handler
  [ctx]
  (let [op-ns   (or (get-in ctx [:params "op-ns"])
                    (get-in ctx [:params :op-ns]))
        op-name (or (get-in ctx [:params "op-name"])
                    (get-in ctx [:params :op-name]))
        op-id   (str op-ns "/" op-name)
        op-fn   (get @!ops op-id)]
    (when-not op-fn
      (throw (ex-info "Unknown live operation"
                      {:op-id op-id})))
    (op-fn ctx)))

(defmacro defsynced
  [name-sym opts]
  (let [path            (:path opts)
        [table id col]  path
        default-value   (:default opts 0)
        entity-type     (or (:entity/type opts)
                            (table->entity-type table))
        table-str       (name table)
        query-col       (col->query-column col)
        query-key       (col->query-key col)
        query-sql       (str "SELECT _id, " query-col
                             " FROM " table-str
                             " WHERE _id = ?")
        query-fn-name   (symbol (str name-sym "-query"))
        extract-fn-name (symbol (str name-sym "-extract-value"))
        value-fn-name   (symbol (str name-sym "-value"))
        spec            {:name name-sym
                         :path path
                         :table table
                         :id id
                         :col col
                         :entity-type entity-type
                         :default default-value
                         :query-fn query-fn-name
                         :extract-fn extract-fn-name
                         :value-fn value-fn-name}]
    (swap! !registry assoc (name name-sym) spec)
    `(do
       (defn ~query-fn-name
         []
         [~query-sql ~id])

       (defn ~extract-fn-name
         [row#]
         (or (get row# ~query-key)
             ~default-value))

       (defn ~value-fn-name
         [ctx#]
         (~extract-fn-name
          (first (live.xtdb/q ctx# (~query-fn-name)))))

       (def ~name-sym
         {:path ~path
          :entity/type ~entity-type
          :query-fn '~query-fn-name
          :extract-fn '~extract-fn-name
          :value-fn '~value-fn-name}))))

(defn- rewrite-swap-form
  [ctx-arg op-name-sym node]
  (let [[_ synced-sym update-fn & update-args] node
        info (get @!registry (name synced-sym))]
    (if-not info
      node
      (let [{:keys [table id col entity-type query-fn extract-fn value-fn]} info]
        `(let [old# (~value-fn ~ctx-arg)
               new# (~update-fn old# ~@update-args)]
           (live.xtdb/put-value-and-publish!
            ~ctx-arg
            {:table ~table
             :doc {:xt/id ~id
                   ~col new#}
             :query (~query-fn)
             :extract-value ~extract-fn
             :expected-value new#
             :changed {:entity/type ~entity-type
                       :entity/id ~id
                       :change/kind :updated}
             :data {:reason ~(keyword (name op-name-sym))}}))))))

(defn- rewrite-operation-form
  [ctx-arg op-name-sym form]
  (walk/postwalk
   (fn [node]
     (if (and (seq? node)
              (= 'swap! (first node))
              (symbol? (second node)))
       (rewrite-swap-form ctx-arg op-name-sym node)
       node))
   form))

(defmacro defoperation
  [name-sym [ctx-arg] & body]
  (let [op-id          (operation-id (ns-name *ns*) name-sym)
        rewritten-body (map #(rewrite-operation-form ctx-arg name-sym %) body)]
    `(do
       (defn ~name-sym
         [~ctx-arg]
         ~@rewritten-body)
       (swap! !ops assoc ~op-id ~name-sym)
       ~name-sym)))
