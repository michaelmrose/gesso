(ns gesso.live.browser.fx
  "Small synchronous FX runner for Gesso Live browser choreography.

   This mirrors the released Biff 2 FX state-machine semantics that Gesso uses
   in portable browser code without depending on JVM-only Biff implementation
   code.

   Compatibility goals:
   - machines are maps of keyword state ids to state functions and start at
     :start
   - :start receives ctx followed by the machine arguments
   - later states receive ctx and the immediately previous output map as a
     separate argument; previous output is never merged into ctx
   - a non-map state result is interpreted as :biff.fx/return
   - effect descriptors are vectors whose first item names a registered handler
   - effect handlers receive the original machine ctx, not the state output
   - :biff.fx/seq evaluates an explicit left-to-right sequence of effect maps or
     effect descriptors and merges their outputs in order
   - :biff.fx/next advances to another local FX state
   - :biff.fx/return returns immediately from the FX machine
   - absence of both :biff.fx/next and :biff.fx/return returns the state output
   - :biff.fx/handlers and :biff.fx/get-handlers supply handlers
   - :biff.fx/now and :biff.fx/seed are injected for each state invocation
   - an optional initial effect descriptor may run before :start, with its result
     passed to :start before the ordinary machine arguments
   - calling a machine with no arguments returns its state map

   This namespace is deliberately smaller than Biff FX. It does not provide
   Biff modules, JVM HTTP defaults, deterministic UUID helpers, schema
   registration, pipelines, or any async facility. Browser async work belongs at
   choreography suspension/event boundaries, not inside this runner."
  (:require
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Public compatibility keys
;; -----------------------------------------------------------------------------

(def handlers-key
  :biff.fx/handlers)

(def get-handlers-key
  :biff.fx/get-handlers)

(def next-key
  :biff.fx/next)

(def return-key
  :biff.fx/return)

(def seq-key
  :biff.fx/seq)

(def now-key
  :biff.fx/now)

(def seed-key
  :biff.fx/seed)

(def state-key
  :biff.fx/state)

(def machine-name-key
  :biff.fx/machine-name)

(def trace-key
  :biff.fx/trace)

;; -----------------------------------------------------------------------------
;; Validation and errors
;; -----------------------------------------------------------------------------

(defn- ex
  ([message data]
   (ex-info message data))
  ([message data cause]
   (ex-info message data cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (ex (str label " must be a map.")
         {:label label
          :value value})))
  value)

(defn- require-function!
  [label value]
  (when-not (ifn? value)
    (throw
     (ex (str label " must be callable.")
         {:label label
          :value value})))
  value)

(defn- valid-state-map?
  [state->fn]
  (and (map? state->fn)
       (ifn? (:start state->fn))
       (every? keyword? (keys state->fn))
       (every? ifn? (vals state->fn))))

(defn- require-state-map!
  [machine-name state->fn]
  (when-not (valid-state-map? state->fn)
    (throw
     (ex "FX machine states must be a keyword->function map containing :start."
         {machine-name-key machine-name
          :biff.fx/states state->fn})))
  state->fn)

(defn- require-handlers!
  [handlers]
  (require-map! "FX handlers" handlers)
  (doseq [[handler-key handler] handlers]
    (when-not (keyword? handler-key)
      (throw
       (ex "FX handler ids must be keywords."
           {:biff.fx/handler handler-key})))
    (require-function! "FX handler" handler))
  handlers)

(defn- throwable-message
  [e]
  (or (.-message e)
      (str e)))

;; -----------------------------------------------------------------------------
;; Browser injections
;; -----------------------------------------------------------------------------

(defn- browser-now
  []
  (js/Date.))

(defn- browser-seed
  "Return a browser-safe random integer for :biff.fx/seed compatibility.

   Released Biff injects a random JVM long. The browser runner does not promise
   identical RNG width; it preserves the presence and integer nature of the
   compatibility key."
  []
  (rand-int 2147483647))

(defn- injected-context
  []
  {now-key (browser-now)
   seed-key (browser-seed)})

;; -----------------------------------------------------------------------------
;; Handler resolution
;; -----------------------------------------------------------------------------

(defn- dynamic-handlers
  [ctx]
  (when-some [get-handlers (get ctx get-handlers-key)]
    (require-function! ":biff.fx/get-handlers" get-handlers)
    (let [handlers (get-handlers)]
      (when (some? handlers)
        (require-map! ":biff.fx/get-handlers result" handlers))
      handlers)))

(defn handlers
  "Resolve the handler map for ctx using released-Biff precedence.

   Explicit :biff.fx/handlers are merged first; handlers returned by
   :biff.fx/get-handlers override them. There are intentionally no browser
   default handlers."
  [ctx]
  (require-map! "FX context" ctx)
  (let [explicit-handlers (get ctx handlers-key)]
    (when (some? explicit-handlers)
      (require-map! "FX handlers" explicit-handlers))
    (-> (merge {}
               explicit-handlers
               (dynamic-handlers ctx))
        require-handlers!)))

;; -----------------------------------------------------------------------------
;; One released-Biff-style state step
;; -----------------------------------------------------------------------------

(defn- effect-vector?
  [handlers value]
  (and (vector? value)
       (contains? handlers
                  (first value))))

(defn- state-function
  ([machine-name state->fn state]
   (state-function machine-name state->fn state nil false))
  ([machine-name state->fn state trace]
   (state-function machine-name state->fn state trace true))
  ([machine-name state->fn state trace include-trace?]
   (or (get state->fn state)
       (throw
        (ex "Invalid state"
            (cond->
             {state-key state
              machine-name-key machine-name
              :biff.fx/available-states (keys state->fn)}
              include-trace?
              (assoc trace-key trace)))))))

(defn- invoke-state
  [machine-name state->fn ctx state input trace]
  (let [state-fn (state-function machine-name state->fn state trace)
        injected (injected-context)]
    (try
      (apply state-fn
             (merge ctx injected)
             input)
      (catch :default e
        (throw
         (ex "State function threw an exception"
             (merge
              {state-key state
               machine-name-key machine-name
               trace-key trace
               :biff.fx/exception-message (throwable-message e)}
              injected)
             e))))))

(defn- normalized-state-result
  [raw-result]
  (if (map? raw-result)
    raw-result
    {return-key raw-result}))

(defn- invoke-handler
  [machine-name state trace ctx result handler-key args handler]
  (try
    (apply handler
           ctx
           args)
    (catch :default e
      (throw
       (ex "Handler function threw an exception"
           {state-key state
            machine-name-key machine-name
            trace-key trace
            :biff.fx/output result
            :biff.fx/handler handler-key
            :biff.fx/handler-args args
            :biff.fx/exception-message (throwable-message e)}
           e)))))

(defn- ignored-effect-key?
  [k]
  (str/starts-with? (str k) ":_"))

(defn- evaluate-effects
  [machine-name state trace handlers ctx result]
  (let [effect-keys
        (filterv
         (fn [k]
           (effect-vector? handlers
                           (get result k)))
         (keys result))]
    (into
     (apply dissoc result effect-keys)
     (keep
      (fn [k]
        (let [[handler-key & args] (get result k)
              handler (get handlers handler-key)
              handler-result
              (invoke-handler machine-name
                              state
                              trace
                              ctx
                              result
                              handler-key
                              args
                              handler)]
          (when-not (ignored-effect-key? k)
            [k handler-result])))
      effect-keys))))

(defn- evaluate-seq-element
  [machine-name state trace handlers ctx element]
  (cond
    (map? element)
    (evaluate-effects machine-name
                      state
                      trace
                      handlers
                      ctx
                      element)

    (effect-vector? handlers element)
    (evaluate-effects machine-name
                      state
                      trace
                      handlers
                      ctx
                      {:_ignored element})

    :else
    (throw
     (ex "Invalid :biff.fx/seq element"
         {state-key state
          machine-name-key machine-name
          trace-key trace
          :biff.fx/element element}))))

(defn- step
  [machine-name state->fn handlers ctx state input trace]
  (let [raw-result
        (invoke-state machine-name
                      state->fn
                      ctx
                      state
                      input
                      trace)

        result
        (normalized-state-result raw-result)

        seq-output
        (mapv
         #(evaluate-seq-element machine-name
                                state
                                trace
                                handlers
                                ctx
                                %)
         (get result seq-key))

        output
        (evaluate-effects machine-name
                          state
                          trace
                          handlers
                          ctx
                          (dissoc result seq-key))]
    (apply merge
           (concat seq-output
                   [output]))))

;; -----------------------------------------------------------------------------
;; Machine construction
;; -----------------------------------------------------------------------------

(defn- parse-machine-args
  [machine-name args]
  (let [[initial-fx args]
        (if (vector? (first args))
          [(first args) (rest args)]
          [nil args])

        state->fn
        (if (and (= 1 (count args))
                 (map? (first args)))
          (first args)
          (apply hash-map args))]
    (when-not (or (nil? initial-fx)
                  (keyword? (first initial-fx)))
      (throw
       (ex "Initial effect must be a vector starting with a keyword."
           {:biff.fx/initial-fx initial-fx
            machine-name-key machine-name})))
    {:initial-fx initial-fx
     :state->fn (require-state-map! machine-name state->fn)}))

(defn- initial-handler
  [handlers handler-key machine-name]
  (or (get handlers handler-key)
      (throw
       (ex "Invalid initial effect handler"
           {:biff.fx/handler handler-key
            machine-name-key machine-name
            :biff.fx/available-handlers (keys handlers)}))))

(defn machine
  "Return a synchronous browser FX machine with released Biff 2 semantics.

   Usage:

     (def save-machine
       (fx/machine
        :example/save
        :start
        (fn [ctx]
          {:result [:example.fx/save (:value ctx)]
           :biff.fx/next :done})
        :done
        (fn [_ctx {:keys [result]}]
          {:biff.fx/return result})))

     (save-machine
      {:value 42
       :biff.fx/handlers
       {:example.fx/save
        (fn [_ctx value]
          value)}})

   Like released Biff FX, the returned function accepts:

     (machine)
       Returns the keyword->state-function map for inspection/testing.

     (machine ctx & args)
       Runs from :start. :start receives ctx followed by args. Every later state
       receives ctx and the immediately previous output map as a separate
       argument.

   An optional initial effect descriptor may be supplied before the state
   definitions. Its handler result is passed to :start before ordinary machine
   arguments.

   This runner is synchronous. Promise/callback completion must be represented
   by choreography suspension and a later event, not by an async FX handler. A
   Promise returned as ordinary state data is therefore just a host value, as
   any other non-map state result is."
  [machine-name & args]
  (let [{:keys [initial-fx state->fn]}
        (parse-machine-args machine-name args)]
    (fn run [& run-args]
      (if (empty? run-args)
        state->fn
        (let [[ctx & machine-args] run-args
              ctx (require-map! "FX context" ctx)
              handlers (handlers ctx)

              initial-result
              (when initial-fx
                (let [handler-key (first initial-fx)
                      handler (initial-handler handlers
                                               handler-key
                                               machine-name)
                      handler-args (rest initial-fx)]
                  [(invoke-handler machine-name
                                   :start
                                   []
                                   ctx
                                   {:biff.fx/initial-fx initial-fx}
                                   handler-key
                                   handler-args
                                   handler)]))

              input
              (concat initial-result
                      machine-args)]
          (loop [state :start
                 input input
                 trace []]
            (let [output
                  (step machine-name
                        state->fn
                        handlers
                        ctx
                        state
                        input
                        trace)]
              (cond
                (get output next-key)
                (do
                  (when (contains? output return-key)
                    (throw
                     (ex "You can't set :biff.fx/next and :biff.fx/return at the same time."
                         {state-key state
                          machine-name-key machine-name
                          :biff.fx/output output})))
                  (recur (get output next-key)
                         [output]
                         (conj trace output)))

                (contains? output return-key)
                (get output return-key)

                :else
                output))))))))
