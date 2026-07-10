(ns gesso.graph
  "An attribute graph resolver and query engine extracted from Biff 2 and
   adapted to Biff 1's module and schema conventions.

   ## Query format

   A query is a vector of attributes:

   - scalar attributes are described with a keyword: `:foo`
   - join attributes are described with a single-entry map, going from a keyword
     to a subquery: `{:foo [:bar]}`
   - optional attributes are described by wrapping them with `[:? ...]`:
     `[:? :foo]`, `{[:? :foo] [:bar]}`

   A join value may be a single map or a sequence of maps. If you do not want
   to enumerate the keys in a join value, use `[:*]` as the join subquery.

   Other EQL features, including union queries and parameters, are not
   supported.

   The module returned by module contributes this namespace's Malli schemas via
   Biff 1's :schema convention and contributes :biff.graph.fx/query through
   :biff.fx/handlers. When query receives a Biff 1 system map, it derives graph
   indexes from :biff/modules unless graph indexes or :biff.graph/get-ctx were
   supplied explicitly."
  (:require [gesso.fx :as fx]
            [malli.core :as m]))

(def query-schema
  [:schema
   {:registry
    {"query"       [:vector [:ref "query-item"]]
     "query-item"  [:orn
                    [:required-or-join [:ref "query-item*"]]
                    [:optional-scalar [:tuple [:= :?] [:ref "attr"]]]]
     "query-item*" [:orn
                    [:scalar [:ref "attr"]]
                    [:join [:and
                            [:map-of
                             [:ref "join-key"]
                             [:ref "subquery"]]
                            [:fn #(= 1 (count %))]]]]
     "join-key"    [:orn
                    [:required [:ref "attr"]]
                    [:optional [:tuple [:= :?] [:ref "attr"]]]]
     "attr"        [:and :keyword [:not [:= :*]]]
     "subquery"    [:orn
                    [:wildcard [:= [:*]]]
                    [:subquery [:ref "query"]]]}}
   [:ref "query"]])

(def ast-schema
  [:schema
   {:registry
    {"ast"      [:map-of [:ref "attr"] [:ref "attr-ast"]]
     "attr"     :keyword
     "attr-ast" [:map {:closed true}
                 [:kind [:enum :scalar :join]]
                 [:optional {:optional true} :boolean]
                 [:wildcard {:optional true} :boolean]
                 [:children {:optional true} [:ref "ast"]]]}}
   [:ref "ast"]])

(def schema
  "Malli schemas contributed by (module)."
  {:biff.graph/id               'qualified-keyword?
   :biff.graph/query            query-schema
   :biff.graph/input-query      :biff.graph/query
   :biff.graph/output-query     :biff.graph/query
   :biff.graph/query-ast        ast-schema
   :biff.graph/input-ast        :biff.graph/query-ast
   :biff.graph/output-ast       :biff.graph/query-ast
   :biff.graph/input            [:or
                                 [:sequential [:maybe 'map?]]
                                 [:maybe 'map?]]
   :biff.graph/batch            :boolean
   :biff.graph/resolve-fn       'ifn?
   :biff.graph/resolver         [:map
                                 [:biff.graph/id]
                                 [:biff.graph/input-ast]
                                 [:biff.graph/output-ast]
                                 [:biff.graph/resolve-fn]
                                 [:biff.graph/batch {:optional true}]]
   :biff.graph/attr             [:and :keyword [:not [:= :*]]]
   :biff.graph/attr->resolvers  [:map-of
                                 :biff.graph/attr
                                 [:sequential :biff.graph/resolver]]
   :biff.graph/attr-shape       [:map
                                 [:kind [:enum :scalar :join]]
                                 [:wildcard {:optional true} :boolean]]
   :biff.graph/attr->shape-info [:map-of
                                 :biff.graph/attr
                                 [:map
                                  [:biff.graph/id {:optional true}]
                                  [:biff.graph/attr]
                                  [:biff.graph/attr-shape]]]
   :biff.graph/cache            [:fn #(or (instance? clojure.lang.IAtom %)
                                          (instance? clojure.lang.Volatile %))]
   :biff.graph/get-ctx          'ifn?
   :biff.graph/middleware       [:sequential 'ifn?]
   :biff.graph/resolvers        [:sequential :biff.graph/resolver]})

(defn validate*
  "Validates with the graph schemas plus schemas available through ctx."
  [m-or-seq & args]
  (let [opts         (if (and (= 1 (count args))
                              (map? (first args)))
                       (first args)
                       (apply hash-map args))
        extra-schema (:extra-schema opts)]
    (apply fx/validate*
           m-or-seq
           (mapcat identity
                   (assoc opts
                          :extra-schema
                          (merge schema extra-schema))))))

(defmacro validate
  "Calls validate* when *assert* is true; otherwise returns m-or-seq unchanged."
  [m-or-seq & opts]
  (if *assert*
    `(validate* ~m-or-seq ~@opts)
    m-or-seq))

(def query-parser
  (m/parser query-schema))

(declare parsed-query-item->ast)

(defn- parsed-subquery->ast
  [parsed-subquery]
  (case (:key parsed-subquery)
    :wildcard {:wildcard true}
    :subquery {:children (into {}
                               (map parsed-query-item->ast)
                               (:value parsed-subquery))}))

(defn- parsed-query-item->ast
  [query-item]
  (let [[query-item optional] (case (:key query-item)
                                :required-or-join [(:value query-item) nil]
                                :optional-scalar [{:key   :scalar
                                                   :value (second (:value query-item))}
                                                  true])]
    (case (:key query-item)
      :scalar
      [(:value query-item) (into {:kind :scalar}
                                 (filter val)
                                 {:optional optional})]

      :join
      (let [[parsed-attr parsed-subquery] (first (:value query-item))
            [attr optional]               (case (:key parsed-attr)
                                            :required [(:value parsed-attr) optional]
                                            :optional [(second (:value parsed-attr)) true])
            {:keys [wildcard children]}   (parsed-subquery->ast parsed-subquery)]
        [attr (into {:kind :join}
                    (filter val)
                    {:children children
                     :optional optional
                     :wildcard wildcard})]))))

(defn query->ast
  "Converts a graph query to its AST representation."
  [query]
  (validate {:biff.graph/query query})
  (into {} (map parsed-query-item->ast) (query-parser query)))

(defn ast-seq [query-ast]
  (->> (tree-seq (constantly true)
                 (comp :children second)
                 [:root {:children query-ast}])
       rest
       (map (fn [[attr info]]
              [attr (select-keys info [:kind :wildcard])]))))

(defn validate-query [{:biff.graph/keys [id query-ast attr->shape-info]}]
  (doseq [[attr shape] (ast-seq query-ast)
          :let         [{expected-shape :biff.graph/attr-shape
                         source-id      :biff.graph/id}
                        (get attr->shape-info attr)]]
    (assert expected-shape
            (str "No resolver declares output for `" attr "` requested by "
                 (or id "query")))
    (assert (= shape expected-shape)
            (str "Got conflicting attr shapes for `" attr "`: "
                 (pr-str shape) " (from " (or id "query") "), "
                 (pr-str expected-shape) " (from " source-id ")"))))

(defn validate-resolver [resolver]
  (validate resolver
            {:required   [:biff.graph/id
                          :biff.graph/input-ast
                          :biff.graph/output-ast
                          :biff.graph/resolve-fn]
             :error-data (select-keys resolver [:biff.graph/id])}))

(defn join-value? [value]
  (or (map? value)
      (and (sequential? value)
           (every? (some-fn map? nil?) value))))

(defn scalar-value? [value]
  (not (join-value? value)))

(defn- validate-input-value [attr->shape-info attr value]
  (when-some [{:biff.graph/keys [attr-shape]} (get attr->shape-info attr)]
    (case (:kind attr-shape)
      :join (assert (join-value? value)
                    (str "Input attr " attr " is a join but value is a scalar"))
      :scalar (assert (scalar-value? value)
                      (str "Input attr " attr " is a scalar but value is a join")))))

(defn validate-input [attr->shape-info input]
  (letfn [(visit [entity]
            (when (map? entity)
              (doseq [[attr value] entity]
                (validate-input-value attr->shape-info attr value)
                (cond
                  (map? value)
                  (visit value)

                  (sequential? value)
                  (run! visit value)))))]
    (run! visit input)))

(defn wrap-input [f]
  (fn [ctx]
    (f ctx (:biff.graph/input ctx))))

(defn resolver
  "Creates a resolver from id, input query, output query, batch flag, and
   two-argument resolve function."
  {:arglists '([{:keys [id input output batch resolve-fn]}])}
  [{:keys [id input output batch resolve-fn]}]
  (validate {:biff.graph/input-query  (or input [])
             :biff.graph/output-query (or output [])}
            {:error-data {:biff.graph/id id}})
  (validate-resolver
   {:biff.graph/id         id
    :biff.graph/input-ast  (query->ast (or input []))
    :biff.graph/output-ast (query->ast (or output []))
    :biff.graph/batch      (boolean batch)
    :biff.graph/resolve-fn (wrap-input resolve-fn)}))

(defmacro defresolver
  "Defines a graph resolver.

   When the first form after opts is an argument vector, the body defines a
   normal two-argument resolver function.

   Otherwise the remaining forms are passed to gesso.fx/machine, with each
   state function wrapped so (:biff.graph/input ctx) is supplied as its second
   argument."
  [sym opts & args]
  (let [use-fx (not (vector? (first args)))
        id     (keyword (str *ns*) (str sym))]
    `(def ~sym
       (let [opts# ~opts]
         (validate
          {:biff.graph/id         ~id
           :biff.graph/input-ast  (query->ast (get opts# :input []))
           :biff.graph/output-ast (query->ast (get opts# :output []))
           :biff.graph/batch      (get opts# :batch false)

           :biff.graph/resolve-fn
           ~(if-not use-fx
              `(wrap-input (fn ~@args))
              `(let [[& {:as state->fn#}] [~@args]]
                 (fx/machine ~id (update-vals state->fn# wrap-input))))})))))

(defn apply-indexed [f indexed]
  (let [indexes (mapv first indexed)
        xs      (mapv second indexed)
        xs      (f xs)]
    (mapv vector indexes xs)))

(defn partition-by-sizes [v sizes]
  (assert (vector? v))
  (assert (vector? sizes))
  (second
   (reduce (fn [[i parts] n]
             (let [j (+ i n)]
               [j (conj parts (subvec v i j))]))
           [0 []]
           sizes)))

(declare resolve-entities)

(defn resolve-attr [{:biff.graph/keys [trace attr->resolvers] :as ctx}
                    entities attr resolving-attrs]
  (assert (vector? entities))
  (if (contains? resolving-attrs attr)
    (vec (repeat (count entities) {::fail-trace trace}))
    (loop [values                      (mapv #(if-some [value (get % attr)]
                                                value
                                                {::fail-trace trace})
                                             entities)
           [resolver & rest-resolvers] (get attr->resolvers attr)]
      (let [indexed-entities
            (into []
                  (comp (map-indexed vector)
                        (filter (comp ::fail-trace values first)))
                  entities)]
        (if (or (empty? indexed-entities) (nil? resolver))
          values
          (let [{:biff.graph/keys [batch resolve-fn]} resolver

                ctx
                (update ctx
                        :biff.graph/trace
                        conj
                        {:resolving (:biff.graph/id resolver)})

                resolve-fn
                (fn [ctx input]
                  (resolve-fn (assoc ctx :biff.graph/input input)))

                batch-resolve-fn
                (if batch
                  resolve-fn
                  (fn [ctx inputs]
                    (mapv #(resolve-fn ctx %) inputs)))

                {resolved-inputs true unresolved-inputs false}
                (->> indexed-entities
                     (apply-indexed #(resolve-entities
                                      ctx
                                      %
                                      (:biff.graph/input-ast resolver)
                                      (conj resolving-attrs attr)))
                     (group-by (comp not ::fail-trace second)))

                indexed-values
                (some->> resolved-inputs
                         not-empty
                         (apply-indexed #(batch-resolve-fn ctx %))
                         (keep (fn [[i result]]
                                 (when (contains? result attr)
                                   [i (get result attr)]))))

                values
                (reduce (fn [values [idx value]]
                          (assoc values idx value))
                        values
                        (into unresolved-inputs indexed-values))]
            (recur values rest-resolvers)))))))

(defn resolve-joins [ctx join-values attr children-ast]
  (assert (every? some? join-values)
          "Join values cannot be nil. Use {} or [] instead.")
  (let [all-maps?        (every? map? join-values)
        all-seqs?        (every? sequential? join-values)
        _                (assert (or all-maps? all-seqs?)
                                 (str "Got conflicting cardinalities for " attr
                                      ". The value should either always be a "
                                      "map or always be a sequence of maps."))
        value-sizes      (when all-seqs? (mapv count join-values))
        flat-join-values (if all-maps?
                           join-values
                           (into [] (mapcat identity) join-values))
        flat-results     (if (empty? flat-join-values)
                           []
                           (resolve-entities ctx flat-join-values children-ast #{}))]
    (if all-maps?
      flat-results
      (mapv (fn [results]
              (if-some [fail-trace (some ::fail-trace results)]
                {::fail-trace fail-trace}
                results))
            (partition-by-sizes flat-results value-sizes)))))

(defn resolve-entities [ctx input query-ast resolving-attrs]
  (reduce (fn [results [attr attr-ast]]
            (let [trace (:biff.graph/trace ctx)
                  trace (update-in trace
                                   [(dec (count trace)) :path]
                                   (fnil conj [])
                                   attr)
                  ctx   (assoc ctx :biff.graph/trace trace)

                  indexed-input
                  (mapv (fn [i input result]
                          [i (merge input result)])
                        (range)
                        input
                        results)

                  indexed-values
                  (->> indexed-input
                       (filterv (comp not ::fail-trace second))
                       (apply-indexed #(resolve-attr ctx
                                                     %
                                                     attr
                                                     resolving-attrs)))

                  indexed-values
                  (if (or (= (:kind attr-ast) :scalar)
                          (:wildcard attr-ast))
                    indexed-values
                    (let [{indexed-resolved true indexed-unresolved false}
                          (group-by (comp not ::fail-trace second)
                                    indexed-values)]
                      (->> indexed-resolved
                           (apply-indexed #(resolve-joins ctx
                                                          %
                                                          attr
                                                          (:children attr-ast)))
                           (into (vec indexed-unresolved)))))

                  idx->value
                  (into {} indexed-values)]
              (mapv (fn [i result]
                      (let [value (get idx->value i)]
                        (cond
                          (::fail-trace result)
                          result

                          (not (::fail-trace value))
                          (assoc result attr value)

                          (:optional attr-ast)
                          result

                          :else
                          (merge result value))))
                    (range)
                    results)))
          (vec (repeat (count input) {}))
          query-ast))

(defn wrap-exception [{:biff.graph/keys [id resolve-fn] :as resolver}]
  (assoc resolver
         :biff.graph/resolve-fn
         (fn [ctx]
           (try
             (resolve-fn ctx)
             (catch Exception e
               (throw
                (ex-info (str "Resolver " id " threw an exception")
                         (select-keys ctx [:biff.graph/trace
                                           :biff.graph/input])
                         e)))))))

(declare select-output)

(defn- select-output-value [value
                            attr
                            {:keys [kind children wildcard]}
                            resolver-id]
  (case kind
    :join (assert (join-value? value)
                  (str resolver-id " declared " attr
                       " as a join but value is a scalar"))
    :scalar (assert (scalar-value? value)
                    (str resolver-id " declared " attr
                         " as a scalar but value is a join")))
  (cond
    (or (= kind :scalar) wildcard)
    value

    (sequential? value)
    (mapv #(select-output (or % {}) children resolver-id) value)

    :else
    (select-output value children resolver-id)))

(defn select-output [output output-ast resolver-id]
  (if (sequential? output)
    (mapv #(select-output % output-ast resolver-id) output)
    (into {}
          (keep (fn [[attr attr-ast]]
                  (when-some [value (get output attr)]
                    [attr (select-output-value value
                                               attr
                                               attr-ast
                                               resolver-id)])))
          output-ast)))

(defn wrap-select-output [{:biff.graph/keys [id resolve-fn output-ast] :as resolver}]
  (let [resolve-fn (fn [ctx]
                     (-> (resolve-fn ctx)
                         (select-output output-ast id)))]
    (assoc resolver :biff.graph/resolve-fn resolve-fn)))

(defn wrap-validate-output [{:biff.graph/keys [id resolve-fn] :as resolver}]
  (assoc resolver
         :biff.graph/resolve-fn
         (fn [ctx]
           (validate (resolve-fn ctx)
                     {:ctx        ctx
                      :error-data {:biff.graph/id id}}))))

(defn- update-cache! [cache f & args]
  (if (instance? clojure.lang.Volatile cache)
    (vreset! cache (apply f @cache args))
    (apply swap! cache f args)))

(defn wrap-cache [{:biff.graph/keys [batch id resolve-fn] :as resolver}]
  (assoc resolver
         :biff.graph/resolve-fn
         (if batch
           (fn [{:biff.graph/keys [cache input] :as ctx}]
             (let [resolver-cache  (when cache
                                     (get @cache id {}))
                   uncached-inputs (when cache
                                     (into []
                                           (comp (remove #(contains? resolver-cache %))
                                                 (distinct))
                                           input))]
               (cond
                 (not cache)
                 (resolve-fn ctx)

                 (empty? uncached-inputs)
                 (mapv resolver-cache input)

                 :else
                 (let [new-results    (resolve-fn (assoc ctx :biff.graph/input uncached-inputs))
                       _              (update-cache! cache update id merge
                                                     (zipmap uncached-inputs new-results))
                       resolver-cache (get @cache id {})]
                   (mapv resolver-cache input)))))
           (fn [{:biff.graph/keys [cache input] :as ctx}]
             (let [resolver-cache (when cache
                                    (get @cache id))]
               (cond
                 (not cache)
                 (resolve-fn ctx)

                 (contains? resolver-cache input)
                 (get resolver-cache input)

                 :else
                 (get-in (update-cache! cache assoc-in [id input] (resolve-fn ctx))
                         [id input])))))))

(defn- shape-info [resolvers]
  (for [resolver     resolvers
        query-ast    [(:biff.graph/input-ast resolver)
                      (:biff.graph/output-ast resolver)]
        [attr shape] (ast-seq query-ast)]
    {:biff.graph/attr       attr
     :biff.graph/attr-shape shape
     :biff.graph/id         (:biff.graph/id resolver)}))

(defn new-ctx
  "Applies middleware to resolvers, validates them, and builds graph indexes."
  {:arglists '([resolvers & {:keys [middleware]}])}
  [resolvers & {:keys [middleware]}]
  (run! validate-resolver resolvers)
  (let [middleware       (into [wrap-cache
                                wrap-validate-output
                                wrap-select-output
                                wrap-exception]
                               middleware)
        resolvers        (mapv (apply comp middleware) resolvers)
        _                (run! validate-resolver resolvers)
        attr->shape-info (into {}
                               (map (juxt :biff.graph/attr identity))
                               (shape-info resolvers))]
    (doseq [resolver  resolvers
            query-key [:biff.graph/input-ast :biff.graph/output-ast]]
      (validate-query
       {:biff.graph/id               (:biff.graph/id resolver)
        :biff.graph/query-ast        (get resolver query-key)
        :biff.graph/attr->shape-info attr->shape-info}))
    {:biff.graph/attr->shape-info
     attr->shape-info

     :biff.graph/attr->resolvers
     (->> (for [resolver resolvers
                attr     (keys (:biff.graph/output-ast resolver))]
            [resolver attr])
          (reduce (fn [acc [resolver attr]]
                    (update acc attr (fnil conj []) resolver))
                  {}))}))

(def ctx-from-modules
  (memoize
   (fn [modules]
     (new-ctx (mapcat :biff.graph/resolvers modules)
              {:middleware (mapcat :biff.graph/middleware modules)}))))

(defn- deref-if-needed [x]
  (if (instance? clojure.lang.IDeref x)
    @x
    x))

(defn- modules-from-ctx [ctx]
  (some-> (:biff/modules ctx)
          deref-if-needed))

(defn query
  "Executes a graph query.

   ctx may be a map returned by new-ctx or a Biff 1 system/request context that
   contains :biff/modules. An explicit :biff.graph/get-ctx takes precedence.

   input may be a map or a sequential collection of maps. Throws when a required
   attribute cannot be resolved."
  ([ctx query*]
   (query ctx {} query*))
  ([{:biff.graph/keys [get-ctx] :as ctx} input query*]
   (validate {:biff.graph/query query*
              :biff.graph/input input}
             {:ctx ctx})
   (let [modules (modules-from-ctx ctx)

         graph-ctx
         (cond
           get-ctx
           (get-ctx)

           (and modules
                (not (and (contains? ctx :biff.graph/attr->resolvers)
                          (contains? ctx :biff.graph/attr->shape-info))))
           (ctx-from-modules modules)

           :else
           nil)

         ctx
         (-> (merge ctx
                    graph-ctx
                    {:biff.graph/cache (volatile! {})
                     :biff.graph/trace [{:resolving :query}]})
             (validate {:ctx      ctx
                        :required [:biff.graph/attr->resolvers
                                   :biff.graph/attr->shape-info]}))

         query-ast
         (query->ast query*)

         _
         (validate-query
          {:biff.graph/attr->shape-info (:biff.graph/attr->shape-info ctx)
           :biff.graph/query-ast        query-ast})

         sequential-input
         (sequential? input)

         input
         (if sequential-input
           (vec input)
           [input])

         _
         (validate-input (:biff.graph/attr->shape-info ctx)
                         input)

         resolving-attrs
         #{}

         results
         (resolve-entities ctx input query-ast resolving-attrs)]
     (doseq [{::keys [fail-trace]} results
             :when                 fail-trace
             :let                  [attr (-> fail-trace
                                             peek
                                             :path
                                             peek)]]
       (throw
        (ex-info (str "Could not resolve " attr)
                 {:biff.graph/trace fail-trace})))
     (if sequential-input
       results
       (first results)))))

(def ^{:doc "A biff.fx handlers map. Contains :biff.graph.fx/query."}
  fx-handlers
  {:biff.graph.fx/query query})

(defn module
  "Returns a Biff 1 module containing Graph schemas and the graph query FX
   handler. Resolver indexes are derived lazily from :biff/modules."
  []
  {:schema           schema
   :biff.fx/handlers fx-handlers})
