(ns gesso.live.browser.fx
  "Small synchronous FX runner for Gesso Live browser choreography.

   This intentionally mirrors the useful core semantics of Biff 2 FX without
   depending on JVM-only Biff implementation code.

   Compatibility goals:
   - machines are maps of keyword state ids to state functions and start at
     :start
   - state functions receive accumulated input merged over the original ctx
   - effect descriptors are vectors whose first item names a registered handler
   - handler results replace their descriptor at the descriptor's map key
   - sequential state results are reduced left-to-right
   - :biff.fx/next advances to another local FX state
   - :biff.fx/return returns immediately from the FX machine
   - absence of both :biff.fx/next and :biff.fx/return returns the state output
   - :biff.fx/handlers and :biff.fx/get-handlers supply handlers
   - :biff.fx/now and :biff.fx/seed are injected for each state invocation

   This namespace is deliberately smaller than Biff FX. It does not provide
   Biff modules, JVM HTTP defaults, UUID helpers, schema registration, or any
   async facility. Browser async work belongs at choreography suspension/event
   boundaries, not inside this runner.")

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

(defn- state-result?
  [value]
  (or (nil? value)
      (map? value)
      (and (sequential? value)
           (every? #(or (nil? %)
                        (map? %))
                   value))))

(defn- require-state-result!
  [machine-name state value]
  (when-not (state-result? value)
    (throw
     (ex "FX state function must return a map, nil, or a sequence of maps/nil."
         {machine-name-key machine-name
          state-key state
          :biff.fx/result value})))
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

   Biff's JVM runner injects a random long. The browser runner does not promise
   identical RNG width; it only preserves the presence and integer nature of
   the compatibility key."
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
  "Resolve the handler map for ctx using Biff-compatible precedence.

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
;; One Biff-style state step
;; -----------------------------------------------------------------------------

(defn- effect-key?
  [handlers result k]
  (let [value (get result k)]
    (and (vector? value)
         (contains? handlers
                    (first value)))))

(defn- state-function
  [machine-name state->fn state]
  (or (get state->fn state)
      (throw
       (ex "Invalid FX state."
           {state-key state
            machine-name-key machine-name
            :biff.fx/available-states (keys state->fn)}))))

(defn- invoke-state
  [machine-name state->fn ctx state input trace]
  (let [state-fn (state-function machine-name state->fn state)
        injected (injected-context)
        state-input (merge ctx input injected)
        result
        (try
          (state-fn state-input)
          (catch :default e
            (throw
             (ex "FX state function threw an exception."
                 (merge
                  {state-key state
                   machine-name-key machine-name
                   trace-key trace
                   :biff.fx/exception-message (throwable-message e)}
                  injected)
                 e))))]
    (require-state-result! machine-name state result)))

(defn- invoke-handler
  [machine-name state trace ctx output handler-key args handler]
  (try
    (apply handler
           (merge ctx output)
           args)
    (catch :default e
      (throw
       (ex "FX handler function threw an exception."
           {state-key state
            machine-name-key machine-name
            trace-key trace
            :biff.fx/output output
            :biff.fx/handler handler-key
            :biff.fx/handler-args args
            :biff.fx/exception-message (throwable-message e)}
           e)))))

(defn- reduce-result
  [machine-name state trace handlers ctx output result]
  (if (nil? result)
    output
    (let [effect-keys (filterv #(effect-key? handlers result %)
                               (keys result))
          output' (merge output
                         (apply dissoc result effect-keys))]
      (into
       output'
       (map
        (fn [k]
          (let [[handler-key & args] (get result k)
                handler (get handlers handler-key)]
            [k
             (invoke-handler machine-name
                             state
                             trace
                             ctx
                             output'
                             handler-key
                             args
                             handler)]))
        effect-keys)))))

(defn- step
  [machine-name state->fn handlers ctx state input trace]
  (let [result (invoke-state machine-name
                             state->fn
                             ctx
                             state
                             input
                             trace)
        results (if (sequential? result)
                  result
                  [result])]
    (reduce
     (fn [output result]
       (reduce-result machine-name
                      state
                      trace
                      handlers
                      ctx
                      output
                      result))
     {}
     results)))

;; -----------------------------------------------------------------------------
;; Machine construction
;; -----------------------------------------------------------------------------

(defn machine
  "Return a synchronous browser FX machine with Biff-2-style semantics.

   Usage:

     (def save-machine
       (fx/machine
        :example/save
        :start
        (fn [ctx]
          {:result [:example.fx/save (:value ctx)]
           :biff.fx/next :done})
        :done
        (fn [{:keys [result]}]
          {:biff.fx/return result})))

     (save-machine
      {:value 42
       :biff.fx/handlers
       {:example.fx/save
        (fn [_ctx value]
          value)}})

   The returned function has the same two useful arities as Biff FX:

     (machine ctx)
       Runs from :start through :biff.fx/next transitions.

     (machine ctx state)
       Calls one raw state function directly with ctx. This intentionally does
       not inject handlers, time, seed, or execute descriptors; it mirrors the
       JVM Biff FX inspection/testing arity.

   This runner is synchronous. Promise/callback completion must be represented
   by choreography suspension and a later event, not by an async FX handler."
  [machine-name & {:as state->fn}]
  (require-state-map! machine-name state->fn)
  (fn run
    ([ctx state]
     ((state-function machine-name state->fn state)
      ctx))
    ([ctx]
     (let [ctx (require-map! "FX context" ctx)
           handlers (handlers ctx)]
       (loop [state :start
              input {}
              trace []]
         (let [output (step machine-name
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
                      (merge input output)
                      (conj trace output)))

             (contains? output return-key)
             (get output return-key)

             :else
             output)))))))
