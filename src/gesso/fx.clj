(ns gesso.fx
  "Effectful state machines extracted from Biff 2 and adapted to Biff 1's
   module and schema conventions.

   Machines have the form:

   (fn [ctx & [state]])

   With one argument, the :start state is executed and transitions continue
   until completion. With a second state argument, that state function is
   called directly without transitions or effects.

   Each state function receives ctx merged with output from the previous state,
   plus fresh :biff.fx/now and :biff.fx/seed values. It returns either a map or
   a sequence of maps. Top-level vector values whose first item names a handler
   are treated as effect descriptors and replaced with handler results.

   Handlers are merged in this order:

   1. The default :biff.fx/http handler.
   2. :biff.fx/handlers from ctx.
   3. :biff.fx/handlers collected from :biff/modules.
   4. The result of :biff.fx/get-handlers, when supplied.

   The module returned by module contributes this namespace's Malli schemas via
   Biff 1's :schema convention."
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.core :as malli]
            [malli.error :as malli.e]
            [malli.registry :as malli.r])
  (:import [java.time Instant]
           [java.util Random UUID]))

(def schema
  "Malli schemas contributed by (module)."
  {:biff.fx/get-handlers 'ifn?
   :biff.fx/handlers     [:map-of :keyword 'ifn?]
   :biff.fx/next         :keyword
   :biff.fx/now          'inst?
   :biff.fx/seed         :int

   ::state->fn
   [:and
    [:map-of :keyword 'ifn?]
    [:map
     [:start 'ifn?]]]

   ::state-fn-result
   [:or
    [:maybe 'map?]
    [:sequential [:maybe 'map?]]]})

(defn- deref-if-needed [x]
  (if (instance? clojure.lang.IDeref x)
    @x
    x))

(defn- modules-from-ctx [ctx]
  (some-> (:biff/modules ctx)
          deref-if-needed))

(defn- schemas-from-modules [ctx]
  (->> (modules-from-ctx ctx)
       (keep :schema)
       (apply merge {})))

(defn- malli-opts-from-ctx [ctx]
  (some-> (:biff/malli-opts ctx)
          deref-if-needed))

(defn assertion-error [& message-parts]
  (throw (AssertionError. (str/join "" message-parts))))

(defn- humanize-explanation [explanation]
  (let [message (-> explanation
                    (update :errors #(take 1 %))
                    malli.e/humanize)]
    (cond
      (malli/validate [:tuple :string] message) (first message)
      (string? message) message
      (nil? message) (pr-str explanation)
      :else (pr-str message))))

(defn- truncate-str [s n]
  (if (<= (count s) n)
    s
    (str (subs s 0 (dec n)) "…")))

(defn- value-str [x]
  (truncate-str (pr-str x) 50))

(defn pprint-str [x]
  (str/trim
   (with-out-str
     (binding [*print-namespace-maps* false]
       (pprint/pprint x)))))

(defn- validate-map
  [m {:keys [required biff-registry malli-registry error-data]}]
  (let [error-data-str (when error-data
                         (str "\n" (pprint-str error-data)))]
    (when-some [missing (not-empty (remove #(contains? m %) required))]
      (assertion-error "Missing required key"
                       (when (< 1 (count missing)) "s")
                       ": "
                       (str/join ", " (mapv pr-str missing))
                       error-data-str))
    (doseq [[k v] (select-keys m (keys biff-registry))
            :when (not (malli/validate k v {:registry malli-registry}))
            :let [explanation (malli/explain k v {:registry malli-registry})
                  message     (humanize-explanation explanation)]]
      (assertion-error "`"
                       (pr-str k)
                       " "
                       (value-str v)
                       "` is invalid: "
                       message
                       error-data-str))))

(defn validate*
  "Validates a map or sequence of maps.

   Validation uses schemas from, in order:

   - modules in (:biff/modules ctx),
   - this namespace's schema map,
   - extra-schema.

   When ctx contains :biff/malli-opts, its registry is retained as the base
   registry so referenced application schemas continue to resolve."
  [m-or-seq & {:keys [ctx extra-schema] :as opts}]
  (let [host-malli-opts (malli-opts-from-ctx ctx)
        biff-registry   (merge (schemas-from-modules ctx)
                               schema
                               extra-schema)
        base-registry   (or (:registry host-malli-opts)
                            malli/default-registry)
        malli-registry  (malli.r/composite-registry
                         (malli.r/fast-registry biff-registry)
                         base-registry)
        opts            (merge opts
                               {:biff-registry  biff-registry
                                :malli-registry malli-registry})]
    (doseq [m (if (sequential? m-or-seq)
                m-or-seq
                [m-or-seq])]
      (cond
        (nil? m)
        nil

        (map? m)
        (validate-map m opts)

        :else
        (assertion-error "Expected a map, got " (value-str m)))))
  m-or-seq)

(defmacro validate
  "Calls validate* when *assert* is true; otherwise returns m-or-seq unchanged."
  [m-or-seq & opts]
  (if *assert*
    `(validate* ~m-or-seq ~@opts)
    m-or-seq))

(def ^:private default-fx-handlers
  {:biff.fx/http
   (fn [_ctx input]
     (let [hato-request (requiring-resolve 'hato.client/request)
           http*        (fn [request]
                          (try
                            (-> (hato-request request)
                                (assoc :url (:url request)))
                            (catch Exception e
                              (if (get request :throw-exceptions true)
                                (throw e)
                                {:url       (:url request)
                                 :exception e}))))]
       (cond
         (nil? hato-request)
         (throw
          (ex-info
           (str "To use :biff.fx/http, you must add hato to your "
                "dependencies.")
           {}))

         (map? input)
         (http* input)

         (sequential? input)
         (mapv http* input)

         :else
         (throw
          (ex-info
           "Invalid input type for :biff.fx/http"
           {:type (type input)})))))})

(defn- truncate [data]
  (walk/postwalk
   #(if (string? %)
      (truncate-str % 500)
      %)
   data))

(defn- step
  [{:keys [machine-name state->fn handlers ctx]}
   {:keys [state input trace]}]
  (let [log-ctx
        {:biff.fx/state        state
         :biff.fx/machine-name machine-name
         :biff.fx/trace        trace}

        error!
        (fn [message extra ex]
          (throw
           (ex-info
            message
            (truncate (merge log-ctx extra))
            ex)))

        state-fn
        (or (get state->fn state)
            (error!
             "Invalid state"
             {:biff.fx/available-states (keys state->fn)}
             nil))

        injected
        {:biff.fx/now  (Instant/now)
         :biff.fx/seed (.nextLong (Random.))}

        state-input
        (merge ctx input injected)

        result
        (try
          (state-fn state-input)
          (catch Exception e
            (error!
             "State function threw an exception"
             injected
             e)))

        _
        (validate
         {::state-fn-result result}
         {:ctx ctx})

        results
        (if (sequential? result)
          result
          [result])]
    (reduce
     (fn [output result]
       (let [effect-keys
             (filterv
              (fn [k]
                (let [v (get result k)]
                  (and (vector? v)
                       (contains? handlers (first v)))))
              (keys result))

             output
             (merge
              output
              (apply dissoc result effect-keys))]
         (into
          output
          (map
           (fn [k]
             (let [[handler-key & args] (get result k)
                   handler              (get handlers handler-key)]
               [k
                (try
                  (apply
                   handler
                   (merge ctx output)
                   args)
                  (catch Exception e
                    (error!
                     "Handler function threw an exception"
                     {:biff.fx/output       output
                      :biff.fx/handler-args args}
                     e)))])))
          effect-keys)))
     {}
     results)))

(def ^:private handlers-for-modules
  (memoize
   (fn [modules]
     (->> modules
          (keep :biff.fx/handlers)
          (apply merge {})))))

(defn machine
  "Returns a function that runs your code as a state machine.

   state->fn
     A map from state keywords to state functions. Must include :start.

   machine-name
     An identifier included in exception data for state-function and handler
     failures.

   Returns (fn [ctx & [state]])."
  [machine-name & {:as state->fn}]
  (validate {::state->fn state->fn})
  (fn run
    ([ctx state]
     ((or
       (get state->fn state)
       (throw
        (ex-info
         "Invalid state"
         {:biff.fx/state            state
          :biff.fx/machine-name     machine-name
          :biff.fx/available-states (keys state->fn)})))
      ctx))

    ([ctx]
     (let [handlers
           (merge
            default-fx-handlers
            (:biff.fx/handlers ctx)
            (handlers-for-modules
             (or (modules-from-ctx ctx) []))
            (when-some [get-handlers
                        (:biff.fx/get-handlers ctx)]
              (get-handlers)))

           _
           (validate
            {:biff.fx/handlers handlers}
            {:ctx ctx})

           opts
           {:machine-name machine-name
            :state->fn    state->fn
            :handlers     handlers
            :ctx          ctx}]
       (loop [state :start
              input {}
              trace []]
         (let [output
               (validate
                (step
                 opts
                 {:state state
                  :input input
                  :trace trace})
                {:ctx ctx})]
           (cond
             (:biff.fx/next output)
             (do
               (assert
                (not
                 (contains? output :biff.fx/return))
                (str
                 "You can't set :biff.fx/next and :biff.fx/return "
                 "at the same time."))
               (recur
                (:biff.fx/next output)
                output
                (conj trace output)))

             (contains? output :biff.fx/return)
             (:biff.fx/return output)

             :else
             output)))))))

(defmacro defmachine
  "Defines a var containing an FX machine. The machine name is constructed from
   the defined symbol and current namespace."
  {:arglists '([sym & {:as state->fn}])}
  [sym & args]
  (let [machine-name (keyword (str *ns*) (str sym))]
    `(def ~sym
       (machine ~machine-name ~@args))))

(defn module
  "Returns a Biff 1 module that contributes the FX schemas.

   Machine handler discovery reads :biff.fx/handlers directly from the modules
   referenced by :biff/modules in ctx, so no system component is required."
  []
  {:schema schema})

(defn uuid
  "Legacy deterministic version-4 UUID generator.

   Returns [uuid next-seed]. Prefer uuid4 for new code."
  [seed]
  (let [rng       (Random. seed)
        msb0      (.nextLong rng)
        lsb0      (.nextLong rng)
        msb       (-> msb0
                      (bit-and
                       (unchecked-long 0xffffffffffff0fff))
                      (bit-or
                       (long 0x4000)))
        lsb       (-> lsb0
                      (bit-and
                       (unchecked-long 0x3fffffffffffffff))
                      (bit-or Long/MIN_VALUE))
        next-seed (.nextLong rng)]
    [(UUID. msb lsb)
     next-seed]))

(defn uuid4
  "Deterministically generates a v4 (random) UUID.

   Returns [uuid next-seed]. For subsequent random operations, use next-seed
   instead of the original seed."
  [seed]
  (let [rng       (Random. seed)
        msb0      (.nextLong rng)
        lsb0      (.nextLong rng)
        msb       (-> msb0
                      (bit-and
                       (unchecked-long 0xffffffffffff0fff))
                      (bit-or
                       (long 0x4000)))
        lsb       (-> lsb0
                      (bit-and
                       (unchecked-long 0x3fffffffffffffff))
                      (bit-or Long/MIN_VALUE))
        next-seed (.nextLong rng)]
    [(UUID. msb lsb)
     next-seed]))

(defn uuid7
  "Deterministically generates a v7 (sequential random) UUID.

   Returns [uuid next-seed]. For subsequent random operations, use next-seed
   instead of the original seed."
  [seed instant]
  (let [rng       (Random. seed)
        ts        (bit-and
                   (inst-ms instant)
                   0xffffffffffff)
        rand-a    (bit-and
                   (.nextInt rng)
                   0x0fff)
        rand-b    (.nextLong rng)
        msb       (unchecked-long
                   (bit-or
                    (bit-shift-left ts 16)
                    (bit-shift-left 0x7 12)
                    rand-a))
        lsb       (-> rand-b
                      (bit-and
                       (unchecked-long 0x3fffffffffffffff))
                      (bit-or Long/MIN_VALUE))
        next-seed (.nextLong rng)]
    [(UUID. msb lsb)
     next-seed]))
