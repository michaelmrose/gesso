(ns gesso.live.browser.fx-conformance-test
  "Cross-platform conformance tests for the Biff-FX subset supported by Gesso's
   browser FX runner. The same cases execute against JVM Biff on Clojure and
   gesso.live.browser.fx on ClojureScript."
  (:require
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])
   #?(:clj [com.biffweb.fx :as fx]
      :cljs [gesso.live.browser.fx :as fx]))
  #?(:clj
     (:import
      [java.util LinkedHashMap])))

(def corpus-version 1)

(def supported-features
  #{:state-transition
    :terminal-return
    :terminal-output
    :nil-state-result
    :sequential-state-result
    :state-input-precedence
    :handler-resolution
    :effect-detection
    :effect-handler-context
    :effect-result
    :runtime-error
    :host-value-pass-through
    :numeric-edge-value})

(def excluded-features
  "Biff features intentionally outside browser-FX differential correspondence.
   Their absence is an architectural boundary, not an untested omission."
  #{:default-http-handler
    :module-handler-collection
    :uuid4
    :uuid7
    :jvm-random-long-width
    :jvm-instant-representation
    :async-handler-completion})

;; -----------------------------------------------------------------------------
;; Host-value vocabulary
;; -----------------------------------------------------------------------------

(defn- host-array
  [values]
  #?(:clj
     (object-array values)
     :cljs
     (into-array values)))

(defn- host-object
  [entries]
  #?(:clj
     (let [m (LinkedHashMap.)]
       (doseq [[k v] entries]
         (.put m (name k) v))
       m)
     :cljs
     (let [obj #js {}]
       (doseq [[k v] entries]
         (aset obj (name k) v))
       obj)))

(defn- host-undefined
  []
  #?(:clj nil
     :cljs js/undefined))

(defn- host-nan
  []
  #?(:clj Double/NaN
     :cljs js/NaN))

(defn- host-positive-infinity
  []
  #?(:clj Double/POSITIVE_INFINITY
     :cljs js/Infinity))

(defn- host-negative-infinity
  []
  #?(:clj Double/NEGATIVE_INFINITY
     :cljs js/-Infinity))

(defn- host-negative-zero
  []
  -0.0)

;; -----------------------------------------------------------------------------
;; Tiny expression language used by cases
;; -----------------------------------------------------------------------------

(declare eval-expr)

(defn- eval-map
  [env m]
  (into (empty m)
        (map (fn [[k v]]
               [k (eval-expr env v)]))
        m))

(defn eval-expr
  "Evaluate one corpus expression.

   Ordinary scalar/map/set values are literals except that map values are walked.
   Vectors beginning with :fx/* are corpus forms. Other vectors are literal
   vectors whose members are recursively evaluated."
  [env expr]
  (if (and (vector? expr)
           (keyword? (first expr))
           (= "fx" (namespace (first expr))))
    (let [[op & args] expr]
      (case op
        :fx/literal
        (first args)

        :fx/ctx
        (get (:ctx env) (first args))

        :fx/arg
        (get (:args env) (first args))

        :fx/contains-ctx?
        (contains? (:ctx env) (first args))

        :fx/get
        (get (eval-expr env (first args))
             (second args))

        :fx/str
        (apply str (map #(eval-expr env %) args))

        :fx/inc
        (inc (eval-expr env (first args)))

        :fx/add
        (reduce + 0 (map #(eval-expr env %) args))

        :fx/vector
        (mapv #(eval-expr env %) args)

        :fx/map
        (->> (partition 2 args)
             (map (fn [[k v]] [k (eval-expr env v)]))
             (into {}))

        :fx/effect
        (let [[handler-id & arg-exprs] args]
          (into [handler-id]
                (map #(eval-expr env %) arg-exprs)))

        :fx/host-array
        (host-array (mapv #(eval-expr env %) args))

        :fx/host-object
        (let [entry-map (eval-expr env (first args))]
          (host-object entry-map))

        :fx/undefined
        (host-undefined)

        :fx/nan
        (host-nan)

        :fx/positive-infinity
        (host-positive-infinity)

        :fx/negative-infinity
        (host-negative-infinity)

        :fx/negative-zero
        (host-negative-zero)

        :fx/throw
        (throw (ex-info (str (eval-expr env (first args)))
                        {:gesso.live.browser.fx-conformance/source
                         (:source env)}))

        (throw
         (ex-info
          "Unknown FX conformance expression."
          {:expression expr
           :operation op}))))
    (cond
      (map? expr)
      (eval-map env expr)

      (vector? expr)
      (mapv #(eval-expr env %) expr)

      (set? expr)
      (into #{} (map #(eval-expr env %)) expr)

      :else
      expr)))

(defn- state-function
  [case-id state state-spec]
  (fn [ctx]
    (eval-expr {:ctx ctx
                :args []
                :source {:case-id case-id
                         :kind :state
                         :state state}}
               (:result state-spec))))

(defn state-functions
  "Compile one case's declarative states into a keyword->function map."
  [case]
  (into {}
        (map (fn [[state state-spec]]
               [state (state-function (:id case) state state-spec)]))
        (:states case)))

(defn- handler-function
  [case-id handler-id handler-spec]
  (fn [ctx & args]
    (eval-expr {:ctx ctx
                :args (vec args)
                :source {:case-id case-id
                         :kind :handler
                         :handler handler-id}}
               (:result handler-spec))))

(defn handler-functions
  "Compile one handler-spec map into a keyword->function map."
  [case handler-specs]
  (into {}
        (map (fn [[handler-id handler-spec]]
               [handler-id
                (handler-function (:id case)
                                  handler-id
                                  handler-spec)]))
        handler-specs))

(defn machine-args
  "Return the vararg tail accepted by both Biff and browser fx/machine."
  [case]
  (->> (state-functions case)
       (sort-by (comp str key))
       (mapcat identity)
       vec))

(defn context
  "Build the machine context for case-data.

   handler-mode controls where the same declarative handlers are installed:
   :explicit, :dynamic, or :precedence. In :precedence mode the case may provide
   :explicit-handlers and :dynamic-handlers separately."
  [case-data]
  (let [base (:ctx case-data)
        mode (get case-data :handler-mode :explicit)]
    (case mode
      :explicit
      (assoc base
             :biff.fx/handlers
             (handler-functions case-data (:handlers case-data)))

      :dynamic
      (assoc base
             :biff.fx/get-handlers
             (fn []
               (handler-functions case-data (:handlers case-data))))

      :precedence
      (assoc base
             :biff.fx/handlers
             (handler-functions case-data (:explicit-handlers case-data))
             :biff.fx/get-handlers
             (fn []
               (handler-functions case-data (:dynamic-handlers case-data))))

      :none
      base

      (throw
       (ex-info
        "Unknown FX conformance handler mode."
        {:case-id (:id case-data)
         :handler-mode mode})))))

;; -----------------------------------------------------------------------------
;; Cross-platform observation normalization
;; -----------------------------------------------------------------------------

(defn- host-array?
  [value]
  #?(:clj
     (and (some? value)
          (.isArray (class value)))
     :cljs
     (array? value)))

(defn- host-object?
  [value]
  #?(:clj
     (and (instance? java.util.Map value)
          (not (map? value)))
     :cljs
     (and (some? value)
          (= "object" (goog/typeOf value))
          (not (array? value))
          (not (coll? value))
          (not (keyword? value))
          (not (symbol? value)))))

(defn- negative-zero?
  [value]
  (and (number? value)
       (zero? value)
       #?(:clj
          (= Double/NEGATIVE_INFINITY
             (/ 1.0 (double value)))
          :cljs
          (= js/-Infinity (/ 1 value)))))

(defn- nan?
  [value]
  #?(:clj
     (and (number? value)
          (Double/isNaN (double value)))
     :cljs
     (and (number? value)
          (js/isNaN value))))

(defn- positive-infinity?
  [value]
  #?(:clj
     (= Double/POSITIVE_INFINITY value)
     :cljs
     (= js/Infinity value)))

(defn- negative-infinity?
  [value]
  #?(:clj
     (= Double/NEGATIVE_INFINITY value)
     :cljs
     (= js/-Infinity value)))

(declare normalize-value)

(defn- normalize-host-object
  [value]
  #?(:clj
     (->> value
          (map (fn [[k v]]
                 [(str k) (normalize-value v)]))
          (sort-by first)
          vec)
     :cljs
     (->> (js/Object.keys value)
          (array-seq)
          (map (fn [k]
                 [k (normalize-value (aget value k))]))
          (sort-by first)
          vec)))

(defn normalize-value
  "Normalize host-specific values into portable EDN.

   JVM nil and JavaScript undefined intentionally share :nilish. The browser
   subset treats undefined handler values as the closest representable Biff
   value rather than granting undefined distinct workflow semantics."
  [value]
  (cond
    (nil? value)
    {:fx/value :nilish}

    #?(:cljs (undefined? value)
       :clj false)
    {:fx/value :nilish}

    (nan? value)
    {:fx/value :nan}

    (negative-zero? value)
    {:fx/value :negative-zero}

    (positive-infinity? value)
    {:fx/value :positive-infinity}

    (negative-infinity? value)
    {:fx/value :negative-infinity}

    (host-array? value)
    {:fx/value :host-array
     :items (mapv normalize-value value)}

    (host-object? value)
    {:fx/value :host-object
     :entries (normalize-host-object value)}

    (map? value)
    (into (empty value)
          (map (fn [[k v]]
                 [k (normalize-value v)]))
          value)

    (vector? value)
    (mapv normalize-value value)

    (set? value)
    (into #{} (map normalize-value) value)

    (sequential? value)
    (mapv normalize-value value)

    :else
    value))

(defn normalize-error
  "Retain only error evidence meaningful on both JVM Biff and browser FX."
  [error]
  (let [data (ex-data error)]
    {:thrown? true
     :state (:biff.fx/state data)
     :machine-name (:biff.fx/machine-name data)
     :trace (normalize-value (:biff.fx/trace data))
     :available-states (some->> (:biff.fx/available-states data)
                                (sort-by str)
                                vec)
     :handler-args (when (contains? data :biff.fx/handler-args)
                     (normalize-value (:biff.fx/handler-args data)))}))

(defn run-case
  "Execute case with machine-constructor and return a portable observation.

   machine-constructor must have the same public calling convention as
   com.biffweb.fx/machine and gesso.live.browser.fx/machine."
  [machine-constructor case]
  (try
    (let [machine (apply machine-constructor
                         (:machine-name case)
                         (machine-args case))
          result (machine (context case))]
      {:status :returned
       :value (normalize-value result)})
    (catch #?(:clj Throwable :cljs :default) error
      {:status :threw
       :error (normalize-error error)})))

(defn expected-observation
  [case]
  (:expect case))

(defn comparable-observation
  "Project an observation to the fields this case claims are correspondence
   obligations. This keeps implementation-specific exception text and host
   details out of the shared contract."
  [case observation]
  (let [keys-to-compare (or (:compare-keys case)
                            (keys (:expect case)))]
    (select-keys observation keys-to-compare)))

;; -----------------------------------------------------------------------------
;; Corpus
;; -----------------------------------------------------------------------------

(def cases
  [
   {:id :transition/basic
    :about "A result sequence feeds an effect and then transitions."
    :features #{:state-transition :sequential-state-result :effect-result}
    :machine-name :conformance/transition-basic
    :ctx {:from-ctx "ctx"}
    :handlers
    {:test/concat
     {:result [:fx/str [:fx/ctx :prefix] [:fx/arg 0]]}}
    :states
    {:start
     {:result
      [{:prefix [:fx/ctx :from-ctx]}
       {:combined [:fx/effect :test/concat [:fx/literal "-effect"]]
        :biff.fx/next :finish}]}
     :finish
     {:result
      {:biff.fx/return
       {:prefix [:fx/ctx :prefix]
        :combined [:fx/ctx :combined]}}}}
    :expect
    {:status :returned
     :value {:prefix "ctx"
             :combined "ctx-effect"}}}

   {:id :transition/only-previous-output-is-carried
    :about "Biff transitions carry the immediately previous state output, not cumulative historical output."
    :features #{:state-transition}
    :machine-name :conformance/previous-output
    :ctx {}
    :handlers {}
    :states
    {:start
     {:result {:from-start :present
               :biff.fx/next :middle}}
     :middle
     {:result {:from-middle :present
               :biff.fx/next :finish}}
     :finish
     {:result {:biff.fx/return
               {:start-visible? [:fx/contains-ctx? :from-start]
                :middle-visible? [:fx/contains-ctx? :from-middle]}}}}
    :expect
    {:status :returned
     :value {:start-visible? false
             :middle-visible? true}}}

   {:id :transition/original-context-remains-visible
    :about "Original context is merged under each state's carried input."
    :features #{:state-transition}
    :machine-name :conformance/original-context
    :ctx {:base 10}
    :handlers {}
    :states
    {:start
     {:result {:derived [:fx/inc [:fx/ctx :base]]
               :biff.fx/next :finish}}
     :finish
     {:result {:biff.fx/return
               {:base [:fx/ctx :base]
                :derived [:fx/ctx :derived]}}}}
    :expect
    {:status :returned
     :value {:base 10
             :derived 11}}}

   {:id :transition/state-output-overrides-original-context
    :about "Carried state output has precedence over the original context."
    :features #{:state-transition :state-input-precedence}
    :machine-name :conformance/output-precedence
    :ctx {:value :original}
    :handlers {}
    :states
    {:start
     {:result {:value :state
               :biff.fx/next :finish}}
     :finish
     {:result {:biff.fx/return [:fx/ctx :value]}}}
    :expect {:status :returned
             :value :state}}

   {:id :transition/injected-keys-override-context
    :about "Per-state now/seed injections override user-provided values."
    :features #{:state-input-precedence}
    :machine-name :conformance/injected-precedence
    :ctx {:biff.fx/now :user-now
          :biff.fx/seed :user-seed}
    :handlers {}
    :states
    {:start
     {:result
      {:biff.fx/return
       {:now-is-user? false
        :seed-is-user? false
        :now-present? [:fx/contains-ctx? :biff.fx/now]
        :seed-present? [:fx/contains-ctx? :biff.fx/seed]}}}}
    :expect
    {:status :returned
     :value {:now-is-user? false
             :seed-is-user? false
             :now-present? true
             :seed-present? true}}}

   {:id :terminal/plain-output
    :about "Without next or return, the state output is the machine result."
    :features #{:terminal-output}
    :machine-name :conformance/plain-output
    :ctx {}
    :handlers {}
    :states {:start {:result {:answer 42}}}
    :expect {:status :returned
             :value {:answer 42}}}

   {:id :terminal/nil-output
    :about "A nil state result contributes an empty output map."
    :features #{:nil-state-result :terminal-output}
    :machine-name :conformance/nil-output
    :ctx {}
    :handlers {}
    :states {:start {:result nil}}
    :expect {:status :returned
             :value {}}}

   {:id :terminal/explicit-nil-return
    :about "A present return key may return nil."
    :features #{:terminal-return}
    :machine-name :conformance/nil-return
    :ctx {}
    :handlers {}
    :states {:start {:result {:biff.fx/return nil}}}
    :expect {:status :returned
             :value {:fx/value :nilish}}}

   {:id :sequence/later-map-overrides-earlier
    :about "Sequential state-result maps are reduced left-to-right."
    :features #{:sequential-state-result}
    :machine-name :conformance/sequence-override
    :ctx {}
    :handlers {}
    :states
    {:start
     {:result [{:value :first :kept 1}
               {:value :second}]}}
    :expect {:status :returned
             :value {:value :second :kept 1}}}

   {:id :sequence/nil-member-is-empty-contribution
    :about "Nil members in a sequential state result are ignored."
    :features #{:sequential-state-result :nil-state-result}
    :machine-name :conformance/sequence-nil
    :ctx {}
    :handlers {}
    :states
    {:start
     {:result [{:a 1} nil {:b 2}]}}
    :expect {:status :returned
             :value {:a 1 :b 2}}}

   {:id :handlers/dynamic-overrides-explicit
    :about "get-handlers takes precedence over explicitly supplied handlers."
    :features #{:handler-resolution :effect-result}
    :machine-name :conformance/handler-precedence
    :ctx {}
    :handler-mode :precedence
    :explicit-handlers
    {:test/source {:result :explicit}}
    :dynamic-handlers
    {:test/source {:result :dynamic}}
    :states
    {:start {:result {:answer [:fx/effect :test/source]}}}
    :expect {:status :returned
             :value {:answer :dynamic}}}

   {:id :handlers/dynamic-only
    :about "A handler supplied only by get-handlers is executable."
    :features #{:handler-resolution :effect-result}
    :machine-name :conformance/dynamic-only
    :ctx {}
    :handler-mode :dynamic
    :handlers {:test/value {:result 7}}
    :states {:start {:result {:answer [:fx/effect :test/value]}}}
    :expect {:status :returned
             :value {:answer 7}}}

   {:id :effects/unregistered-vector-remains-data
    :about "A vector is an effect only when its first item names a registered handler."
    :features #{:effect-detection}
    :machine-name :conformance/unregistered-vector
    :ctx {}
    :handlers {}
    :states
    {:start
     {:result {:payload [:fx/literal [:test/not-a-handler 41]]}}}
    :expect {:status :returned
             :value {:payload [:test/not-a-handler 41]}}}

   {:id :effects/registered-vector-executes
    :about "A vector whose first item names a registered handler is replaced by the handler result."
    :features #{:effect-detection :effect-result}
    :machine-name :conformance/registered-vector
    :ctx {}
    :handlers {:test/inc {:result [:fx/inc [:fx/arg 0]]}}
    :states {:start {:result {:answer [:fx/effect :test/inc 41]}}}
    :expect {:status :returned
             :value {:answer 42}}}

   {:id :effects/handler-sees-original-context
    :about "Effect handlers receive original machine context."
    :features #{:effect-handler-context}
    :machine-name :conformance/handler-original-context
    :ctx {:base "ctx"}
    :handlers
    {:test/read {:result [:fx/str [:fx/ctx :base] [:fx/arg 0]]}}
    :states
    {:start {:result {:answer [:fx/effect :test/read "-ok"]}}}
    :expect {:status :returned
             :value {:answer "ctx-ok"}}}

   {:id :effects/handler-sees-current-non-effect-output
    :about "Before effects run, non-effect fields from the same result map are visible to handlers."
    :features #{:effect-handler-context}
    :machine-name :conformance/handler-current-output
    :ctx {}
    :handlers
    {:test/read-prefix {:result [:fx/str [:fx/ctx :prefix] [:fx/arg 0]]}}
    :states
    {:start
     {:result {:prefix "p"
               :answer [:fx/effect :test/read-prefix "-x"]}}}
    :expect {:status :returned
             :value {:prefix "p"
                     :answer "p-x"}}}

   {:id :effects/later-sequence-effect-sees-prior-output
    :about "An effect in a later result-map sees output accumulated from earlier result-maps in the same state."
    :features #{:effect-handler-context :sequential-state-result}
    :machine-name :conformance/handler-prior-result
    :ctx {}
    :handlers
    {:test/read-prefix {:result [:fx/str [:fx/ctx :prefix] [:fx/arg 0]]}}
    :states
    {:start
     {:result [{:prefix "p"}
               {:answer [:fx/effect :test/read-prefix "-later"]}]}}
    :expect {:status :returned
             :value {:prefix "p"
                     :answer "p-later"}}}

   {:id :effects/sibling-handlers-do-not-require-order
    :about "Sibling effects are compared only by their independent results; neither may depend on sibling execution order."
    :features #{:effect-handler-context}
    :machine-name :conformance/sibling-effects
    :ctx {:base 5}
    :handlers
    {:test/add {:result [:fx/add [:fx/ctx :base] [:fx/arg 0]]}
     :test/inc {:result [:fx/inc [:fx/arg 0]]}}
    :states
    {:start
     {:result {:left [:fx/effect :test/add 2]
               :right [:fx/effect :test/inc 9]}}}
    :expect {:status :returned
             :value {:left 7 :right 10}}}

   {:id :host/array-result
    :about "Opaque host arrays may pass through an effect result without becoming FX structure."
    :features #{:host-value-pass-through}
    :machine-name :conformance/host-array
    :ctx {}
    :handlers
    {:test/array {:result [:fx/host-array 1 "two" nil]}}
    :states {:start {:result {:value [:fx/effect :test/array]}}}
    :expect
    {:status :returned
     :value {:value {:fx/value :host-array
                     :items [1 "two" {:fx/value :nilish}]}}}}

   {:id :host/object-result
    :about "Opaque host objects may pass through an effect result without being interpreted as state output."
    :features #{:host-value-pass-through}
    :machine-name :conformance/host-object
    :ctx {}
    :handlers
    {:test/object
     {:result [:fx/host-object
               {:answer 42
                :label "object"}]}}
    :states {:start {:result {:value [:fx/effect :test/object]}}}
    :expect
    {:status :returned
     :value {:value {:fx/value :host-object
                     :entries [["answer" 42]
                               ["label" "object"]]}}}}

   {:id :host/undefined-is-nilish
    :about "JavaScript undefined is normalized to the nil-like reference value."
    :features #{:host-value-pass-through}
    :machine-name :conformance/undefined
    :ctx {}
    :handlers {:test/value {:result [:fx/undefined]}}
    :states {:start {:result {:value [:fx/effect :test/value]}}}
    :expect {:status :returned
             :value {:value {:fx/value :nilish}}}}

   {:id :numeric/nan
    :about "NaN survives as an opaque handler value."
    :features #{:numeric-edge-value}
    :machine-name :conformance/nan
    :ctx {}
    :handlers {:test/value {:result [:fx/nan]}}
    :states {:start {:result {:value [:fx/effect :test/value]}}}
    :expect {:status :returned
             :value {:value {:fx/value :nan}}}}

   {:id :numeric/positive-infinity
    :about "Positive infinity survives as an opaque handler value."
    :features #{:numeric-edge-value}
    :machine-name :conformance/positive-infinity
    :ctx {}
    :handlers {:test/value {:result [:fx/positive-infinity]}}
    :states {:start {:result {:value [:fx/effect :test/value]}}}
    :expect {:status :returned
             :value {:value {:fx/value :positive-infinity}}}}

   {:id :numeric/negative-infinity
    :about "Negative infinity survives as an opaque handler value."
    :features #{:numeric-edge-value}
    :machine-name :conformance/negative-infinity
    :ctx {}
    :handlers {:test/value {:result [:fx/negative-infinity]}}
    :states {:start {:result {:value [:fx/effect :test/value]}}}
    :expect {:status :returned
             :value {:value {:fx/value :negative-infinity}}}}

   {:id :numeric/negative-zero
    :about "Negative zero remains distinguishable from positive zero."
    :features #{:numeric-edge-value}
    :machine-name :conformance/negative-zero
    :ctx {}
    :handlers {:test/value {:result [:fx/negative-zero]}}
    :states {:start {:result {:value [:fx/effect :test/value]}}}
    :expect {:status :returned
             :value {:value {:fx/value :negative-zero}}}}

   {:id :numeric/max-safe-integer
    :about "The largest exactly representable JavaScript integer is portable."
    :features #{:numeric-edge-value}
    :machine-name :conformance/max-safe-integer
    :ctx {}
    :handlers {:test/value {:result 9007199254740991}}
    :states {:start {:result {:value [:fx/effect :test/value]}}}
    :expect {:status :returned
             :value {:value 9007199254740991}}}

   {:id :error/state-function-throws
    :about "A state-function exception is surfaced with state and machine identity."
    :features #{:runtime-error}
    :machine-name :conformance/state-throws
    :ctx {}
    :handlers {}
    :states
    {:start {:result [:fx/throw "state boom"]}}
    :expect
    {:status :threw
     :error {:thrown? true
             :state :start
             :machine-name :conformance/state-throws
             :trace []
             :available-states nil
             :handler-args nil}}}

   {:id :error/handler-throws
    :about "A handler exception is surfaced with state, machine, trace and handler arguments."
    :features #{:runtime-error}
    :machine-name :conformance/handler-throws
    :ctx {}
    :handlers
    {:test/fail {:result [:fx/throw "handler boom"]}}
    :states
    {:start {:result {:answer [:fx/effect :test/fail "arg"]}}}
    :expect
    {:status :threw
     :error {:thrown? true
             :state :start
             :machine-name :conformance/handler-throws
             :trace []
             :available-states nil
             :handler-args ["arg"]}}}

   {:id :error/transition-to-missing-state
    :about "A transition to an unknown state is rejected with portable state identity."
    :features #{:runtime-error :state-transition}
    :machine-name :conformance/missing-state
    :ctx {}
    :handlers {}
    :states {:start {:result {:biff.fx/next :missing}}}
    :expect
    {:status :threw
     :error {:thrown? true
             :state :missing
             :machine-name :conformance/missing-state
             :trace [{:biff.fx/next :missing}]
             :available-states [:start]
             :handler-args nil}}}

   {:id :error/next-and-return
    :about "A state output cannot both transition and return."
    :features #{:runtime-error :state-transition :terminal-return}
    :machine-name :conformance/next-and-return
    :ctx {}
    :handlers {}
    :states
    {:start {:result {:biff.fx/next :finish
                      :biff.fx/return :nope}}
     :finish {:result {:biff.fx/return :done}}}
    :compare-keys [:status]
    :expect {:status :threw}}
   ])


(defn case-by-id
  [case-id]
  (some #(when (= case-id (:id %)) %) cases))

(defn corpus-errors
  "Return structural errors in the shared corpus itself."
  []
  (let [ids (map :id cases)
        duplicated-ids (->> ids frequencies (keep (fn [[id n]] (when (> n 1) id))) vec)]
    (cond-> []
      (seq duplicated-ids)
      (conj {:error :duplicate-case-ids
             :ids duplicated-ids})

      (some #(not (keyword? (:id %))) cases)
      (conj {:error :non-keyword-case-id})

      (some #(not (map? (:states %))) cases)
      (conj {:error :invalid-states})

      (some #(not (contains? (:states %) :start)) cases)
      (conj {:error :missing-start-state})

      (some #(not (map? (:expect %))) cases)
      (conj {:error :invalid-expectation})

      (some #(not (every? supported-features (:features %))) cases)
      (conj {:error :unknown-feature}))))

(defn valid-corpus?
  []
  (empty? (corpus-errors)))

;; -----------------------------------------------------------------------------
;; Conformance gate
;; -----------------------------------------------------------------------------

(def implementation-label
  #?(:clj "Biff FX"
     :cljs "Browser FX"))

(defn- observation
  [case-data]
  (->> case-data
       (run-case fx/machine)
       (comparable-observation case-data)))

(defn- expected
  [case-data]
  (->> case-data
       expected-observation
       (comparable-observation case-data)))

(defn- divergence
  [case-data]
  (let [expected' (expected case-data)
        actual' (observation case-data)]
    (when-not (= expected' actual')
      {:id (:id case-data)
       :about (:about case-data)
       :expected expected'
       :actual actual'})))

(deftest corpus-is-structurally-valid-test
  (testing "the conformance gate never runs a malformed case corpus"
    (is (true? (valid-corpus?))
        (pr-str (corpus-errors)))
    (is (= 1 corpus-version))
    (is (= 30 (count cases)))))

(deftest machine-matches-corpus-test
  (doseq [case-data cases]
    (testing (str (:id case-data) " — " (:about case-data))
      (is (= (expected case-data)
             (observation case-data))
          (str implementation-label " divergence for "
               (:id case-data)
               "\nexpected: " (pr-str (expected case-data))
               "\nactual:   " (pr-str (observation case-data)))))))

(deftest transition-carries-only-immediately-previous-output-test
  (let [case-data (case-by-id
                   :transition/only-previous-output-is-carried)]
    (testing "FX transitions carry only the immediately previous state output"
      (is (some? case-data))
      (is (= {:status :returned
              :value {:start-visible? false
                      :middle-visible? true}}
             (observation case-data))))))

(deftest missing-state-retains-transition-trace-test
  (let [case-data (case-by-id
                   :error/transition-to-missing-state)]
    (testing "a missing transition destination retains the prior trace"
      (is (some? case-data))
      (is (= {:status :threw
              :error {:thrown? true
                      :state :missing
                      :machine-name :conformance/missing-state
                      :trace [{:biff.fx/next :missing}]
                      :available-states [:start]
                      :handler-args nil}}
             (observation case-data))))))

(deftest divergence-report-is-empty-test
  (let [divergences (->> cases
                         (keep divergence)
                         vec)]
    (is (empty? divergences)
        (str implementation-label " conformance divergences:\n"
             (with-out-str
               (doseq [item divergences]
                 (prn item)))))))
