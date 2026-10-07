(ns gesso.live.browser.fx-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.fx :as fx]))

;; Browser-specific implementation tests.
;;
;; Cross-platform correspondence with released com.biffweb/fx belongs in
;; gesso.live.browser.fx-conformance-test.  This namespace concentrates on the
;; browser runner's public compatibility keys, validation, diagnostics,
;; synchronous-only contract, and browser-specific now/seed injections.

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-data
  [f]
  (some-> (thrown f)
          ex-data))

(defn- date?
  [value]
  (instance? js/Date value))

;; -----------------------------------------------------------------------------
;; Compatibility keys
;; -----------------------------------------------------------------------------

(deftest compatibility-key-test
  (is (= :biff.fx/handlers fx/handlers-key))
  (is (= :biff.fx/get-handlers fx/get-handlers-key))
  (is (= :biff.fx/next fx/next-key))
  (is (= :biff.fx/return fx/return-key))
  (is (= :biff.fx/seq fx/seq-key))
  (is (= :biff.fx/now fx/now-key))
  (is (= :biff.fx/seed fx/seed-key))
  (is (= :biff.fx/state fx/state-key))
  (is (= :biff.fx/machine-name fx/machine-name-key))
  (is (= :biff.fx/trace fx/trace-key)))

;; -----------------------------------------------------------------------------
;; Handler resolution
;; -----------------------------------------------------------------------------

(deftest handlers-default-to-empty-map-test
  (is (= {} (fx/handlers {}))))

(deftest explicit-handlers-are-returned-test
  (let [handler (fn [_ctx] :handled)]
    (is (= {:test/handler handler}
           (fx/handlers
            {:biff.fx/handlers
             {:test/handler handler}})))))

(deftest dynamic-handlers-override-explicit-handlers-test
  (let [explicit (fn [_ctx] :explicit)
        dynamic (fn [_ctx] :dynamic)
        extra (fn [_ctx] :extra)
        calls (atom 0)
        resolved
        (fx/handlers
         {:biff.fx/handlers
          {:test/shared explicit
           :test/explicit-only explicit}
          :biff.fx/get-handlers
          (fn []
            (swap! calls inc)
            {:test/shared dynamic
             :test/dynamic-only extra})})]
    (is (= 1 @calls))
    (is (identical? dynamic (:test/shared resolved)))
    (is (identical? explicit (:test/explicit-only resolved)))
    (is (identical? extra (:test/dynamic-only resolved)))))

(deftest nil-dynamic-handler-result-contributes-nothing-test
  (let [explicit (fn [_ctx] :explicit)]
    (is (= {:test/handler explicit}
           (fx/handlers
            {:biff.fx/handlers {:test/handler explicit}
             :biff.fx/get-handlers (fn [] nil)})))))

(deftest handler-resolution-validates-context-test
  (let [data (thrown-data #(fx/handlers [:not :a-map]))]
    (is (= "FX context" (:label data)))
    (is (= [:not :a-map] (:value data)))))

(deftest explicit-handlers-must-be-map-test
  (let [data (thrown-data
              #(fx/handlers
                {:biff.fx/handlers [:not :a-map]}))]
    (is (= "FX handlers" (:label data)))
    (is (= [:not :a-map] (:value data)))))

(deftest dynamic-handler-provider-must-be-callable-test
  (let [data (thrown-data
              #(fx/handlers
                {:biff.fx/get-handlers 42}))]
    (is (= ":biff.fx/get-handlers" (:label data)))
    (is (= 42 (:value data)))))

(deftest dynamic-handler-provider-must-return-map-or-nil-test
  (let [data (thrown-data
              #(fx/handlers
                {:biff.fx/get-handlers
                 (fn [] [:not :a-map])}))]
    (is (= ":biff.fx/get-handlers result" (:label data)))
    (is (= [:not :a-map] (:value data)))))

(deftest handler-ids-must-be-keywords-test
  (let [data (thrown-data
              #(fx/handlers
                {:biff.fx/handlers
                 {"not-a-keyword" (fn [_ctx] nil)}}))]
    (is (= "not-a-keyword" (:biff.fx/handler data)))))

(deftest handlers-must-be-callable-test
  (let [data (thrown-data
              #(fx/handlers
                {:biff.fx/handlers
                 {:test/not-callable 42}}))]
    (is (= "FX handler" (:label data)))
    (is (= 42 (:value data)))))

;; -----------------------------------------------------------------------------
;; Machine construction and inspection
;; -----------------------------------------------------------------------------

(deftest machine-requires-start-state-test
  (let [data (thrown-data
              #(fx/machine
                :test/no-start
                :finish (fn [_ctx _previous] {})))]
    (is (= :test/no-start (:biff.fx/machine-name data)))
    (is (map? (:biff.fx/states data)))))

(deftest machine-state-ids-must-be-keywords-test
  (let [data (thrown-data
              #(apply fx/machine
                      :test/bad-state-id
                      ["start" (fn [_ctx] {})]))]
    (is (= :test/bad-state-id (:biff.fx/machine-name data)))))

(deftest machine-state-values-must-be-callable-test
  (let [data (thrown-data
              #(fx/machine
                :test/bad-state-value
                :start 42))]
    (is (= :test/bad-state-value (:biff.fx/machine-name data)))))

(deftest machine-may-be-built-from-state-map-test
  (let [start-fn (fn [_ctx] :done)
        finish-fn (fn [_ctx _previous] :finished)
        states {:start start-fn
                :finish finish-fn}
        machine (fx/machine :test/state-map states)]
    (is (= states (machine)))
    (is (identical? start-fn (:start (machine))))
    (is (identical? finish-fn (:finish (machine))))))

(deftest zero-argument-machine-call-returns-state-map-without-running-test
  (let [calls (atom 0)
        start-fn (fn [_ctx]
                   (swap! calls inc)
                   :done)
        machine (fx/machine
                 :test/inspect
                 :start start-fn)]
    (is (= {:start start-fn} (machine)))
    (is (= 0 @calls))))

(deftest invalid-initial-effect-shape-is-rejected-test
  (let [data (thrown-data
              #(fx/machine
                :test/bad-initial
                ["not-a-keyword"]
                :start (fn [_ctx _initial] :done)))]
    (is (= :test/bad-initial (:biff.fx/machine-name data)))
    (is (= ["not-a-keyword"] (:biff.fx/initial-fx data)))))

;; -----------------------------------------------------------------------------
;; Released Biff 2 value flow
;; -----------------------------------------------------------------------------

(deftest start-receives-machine-arguments-after-context-test
  (let [seen (atom nil)
        machine
        (fx/machine
         :test/machine-args
         :start
         (fn [ctx left right]
           (reset! seen [ctx left right])
           :done))
        ctx {:request/id 7}]
    (is (= :done (machine ctx :left :right)))
    (let [[seen-ctx left right] @seen]
      (is (= 7 (:request/id seen-ctx)))
      (is (= :left left))
      (is (= :right right)))))

(deftest later-state-receives-original-context-and-separate-previous-output-test
  (let [seen (atom nil)
        ctx {:request/id 7
             :shared :original}
        machine
        (fx/machine
         :test/previous-output
         :start
         (fn [_ctx]
           {:shared :state-output
            :value 41
            :biff.fx/next :finish})
         :finish
         (fn [finish-ctx previous-output]
           (reset! seen [finish-ctx previous-output])
           (:value previous-output)))]
    (is (= 41 (machine ctx)))
    (let [[finish-ctx previous-output] @seen]
      (is (= 7 (:request/id finish-ctx)))
      (is (= :original (:shared finish-ctx)))
      (is (not (contains? finish-ctx :value)))
      (is (= :state-output (:shared previous-output)))
      (is (= 41 (:value previous-output)))
      (is (= :finish (:biff.fx/next previous-output))))))

(deftest non-map-state-results-are-terminal-return-values-test
  (doseq [value [nil 42 "value" :keyword [1 2 3]]]
    (let [machine
          (fx/machine
           :test/non-map-return
           :start (fn [_ctx] value))]
      (is (= value (machine {}))))))

(deftest map-without-control-keys-is-returned-as-output-test
  (let [machine
        (fx/machine
         :test/output
         :start
         (fn [_ctx]
           {:answer 42
            :plain true}))]
    (is (= {:answer 42
            :plain true}
           (machine {})))))

(deftest next-and-return-together-are-rejected-test
  (let [machine
        (fx/machine
         :test/ambiguous-control
         :start
         (fn [_ctx]
           {:biff.fx/next :finish
            :biff.fx/return :done})
         :finish
         (fn [_ctx _previous]
           :finish))
        error (thrown #(machine {}))
        data (ex-data error)]
    (is (= "You can't set :biff.fx/next and :biff.fx/return at the same time."
           (ex-message error)))
    (is (= :start (:biff.fx/state data)))
    (is (= :test/ambiguous-control (:biff.fx/machine-name data)))
    (is (= :finish
           (get-in data [:biff.fx/output :biff.fx/next])))
    (is (= :done
           (get-in data [:biff.fx/output :biff.fx/return])))))

(deftest transition-to-unknown-state-is-diagnostic-test
  (let [machine
        (fx/machine
         :test/missing-state
         :start
         (fn [_ctx]
           {:biff.fx/next :missing}))
        error (thrown #(machine {}))
        data (ex-data error)]
    (is (= "Invalid state" (ex-message error)))
    (is (= :missing (:biff.fx/state data)))
    (is (= :test/missing-state (:biff.fx/machine-name data)))
    (is (= [:start]
           (vec (:biff.fx/available-states data))))
    (is (= [{:biff.fx/next :missing}]
           (:biff.fx/trace data)))))

;; -----------------------------------------------------------------------------
;; Effects and :biff.fx/seq
;; -----------------------------------------------------------------------------

(deftest effect-handler-receives-original-context-test
  (let [seen (atom nil)
        machine
        (fx/machine
         :test/effect-context
         :start
         (fn [_ctx]
           {:state-only :not-handler-context
            :answer [:test/add 20 22]}))
        ctx
        {:request/id 7
         :biff.fx/handlers
         {:test/add
          (fn [handler-ctx left right]
            (reset! seen handler-ctx)
            (+ left right))}}]
    (is (= {:state-only :not-handler-context
            :answer 42}
           (machine ctx)))
    (is (= 7 (:request/id @seen)))
    (is (not (contains? @seen :state-only)))
    (is (contains? @seen :biff.fx/handlers))))

(deftest unknown-effect-looking-vector-remains-ordinary-data-test
  (let [machine
        (fx/machine
         :test/unknown-vector
         :start
         (fn [_ctx]
           {:value [:test/not-registered 1 2]}))]
    (is (= {:value [:test/not-registered 1 2]}
           (machine {})))))

(deftest underscore-prefixed-effect-output-is-discarded-test
  (let [calls (atom [])
        machine
        (fx/machine
         :test/ignored-effect
         :start
         (fn [_ctx]
           {:_side-effect [:test/record :ran]
            :kept :value}))
        result
        (machine
         {:biff.fx/handlers
          {:test/record
           (fn [_ctx value]
             (swap! calls conj value)
             :handler-result)}})]
    (is (= [:ran] @calls))
    (is (= {:kept :value} result))
    (is (not (contains? result :_side-effect)))))

(deftest explicit-seq-runs-left-to-right-and-merges-output-test
  (let [calls (atom [])
        machine
        (fx/machine
         :test/seq
         :start
         (fn [_ctx]
           {:biff.fx/seq
            [{:a [:test/record :first]}
             [:test/record :middle]
             {:a [:test/record :last]
              :b 2}]
            :ordinary :wins-last}))
        result
        (machine
         {:biff.fx/handlers
          {:test/record
           (fn [_ctx value]
             (swap! calls conj value)
             value)}})]
    (is (= [:first :middle :last] @calls))
    (is (= {:a :last
            :b 2
            :ordinary :wins-last}
           result))))

(deftest ordinary-output-overrides-seq-output-test
  (let [machine
        (fx/machine
         :test/seq-precedence
         :start
         (fn [_ctx]
           {:biff.fx/seq
            [{:answer [:test/id :from-seq]}]
            :answer :ordinary}))]
    (is (= {:answer :ordinary}
           (machine
            {:biff.fx/handlers
             {:test/id (fn [_ctx value] value)}})))))

(deftest invalid-seq-element-has-machine-diagnostic-test
  (let [machine
        (fx/machine
         :test/bad-seq
         :start
         (fn [_ctx]
           {:biff.fx/seq [42]}))
        error (thrown #(machine {}))
        data (ex-data error)]
    (is (= "Invalid :biff.fx/seq element" (ex-message error)))
    (is (= :test/bad-seq (:biff.fx/machine-name data)))
    (is (= :start (:biff.fx/state data)))
    (is (= 42 (:biff.fx/element data)))
    (is (= [] (:biff.fx/trace data)))))

;; -----------------------------------------------------------------------------
;; Initial effects
;; -----------------------------------------------------------------------------

(deftest initial-effect-result-precedes-machine-arguments-test
  (let [seen (atom nil)
        initial-seen (atom nil)
        machine
        (fx/machine
         :test/initial
         [:test/prepare 40]
         :start
         (fn [ctx initial-result ordinary-arg]
           (reset! seen [ctx initial-result ordinary-arg])
           (+ initial-result ordinary-arg)))
        ctx
        {:request/id 7
         :biff.fx/handlers
         {:test/prepare
          (fn [handler-ctx value]
            (reset! initial-seen handler-ctx)
            (+ value 1))}}]
    (is (= 42 (machine ctx 1)))
    (let [[start-ctx initial-result ordinary-arg] @seen]
      (is (= 7 (:request/id start-ctx)))
      (is (= 41 initial-result))
      (is (= 1 ordinary-arg)))
    (is (= 7 (:request/id @initial-seen)))))

(deftest missing-initial-effect-handler-is-diagnostic-test
  (let [machine
        (fx/machine
         :test/missing-initial-handler
         [:test/missing 1]
         :start
         (fn [_ctx _initial] :done))
        error (thrown #(machine {}))
        data (ex-data error)]
    (is (= "Invalid initial effect handler" (ex-message error)))
    (is (= :test/missing (:biff.fx/handler data)))
    (is (= :test/missing-initial-handler
           (:biff.fx/machine-name data)))
    (is (= []
           (vec (:biff.fx/available-handlers data))))))

;; -----------------------------------------------------------------------------
;; Browser-only per-state injections
;; -----------------------------------------------------------------------------

(deftest machine-injects-now-and-seed-test
  (let [seen (atom nil)
        machine
        (fx/machine
         :test/injections
         :start
         (fn [ctx]
           (reset! seen
                   {:now (:biff.fx/now ctx)
                    :seed (:biff.fx/seed ctx)})
           :done))]
    (is (= :done (machine {})))
    (is (date? (:now @seen)))
    (is (integer? (:seed @seen)))
    (is (<= 0 (:seed @seen)))
    (is (< (:seed @seen) 2147483647))))

(deftest injections-override-user-supplied-now-and-seed-test
  (let [seen (atom nil)
        machine
        (fx/machine
         :test/injection-precedence
         :start
         (fn [ctx]
           (reset! seen
                   {:now (:biff.fx/now ctx)
                    :seed (:biff.fx/seed ctx)})
           :done))]
    (machine {:biff.fx/now :fake-now
              :biff.fx/seed :fake-seed})
    (is (date? (:now @seen)))
    (is (not= :fake-now (:now @seen)))
    (is (integer? (:seed @seen)))
    (is (not= :fake-seed (:seed @seen)))))

(deftest each-state-receives-browser-injections-test
  (let [seen (atom [])
        machine
        (fx/machine
         :test/per-state-injection
         :start
         (fn [ctx]
           (swap! seen conj
                  [(:biff.fx/now ctx)
                   (:biff.fx/seed ctx)])
           {:biff.fx/next :finish})
         :finish
         (fn [ctx _previous]
           (swap! seen conj
                  [(:biff.fx/now ctx)
                   (:biff.fx/seed ctx)])
           :done))]
    (is (= :done (machine {})))
    (is (= 2 (count @seen)))
    (is (every?
         (fn [[now seed]]
           (and (date? now)
                (integer? seed)))
         @seen))))

;; -----------------------------------------------------------------------------
;; Browser-local exception diagnostics
;; -----------------------------------------------------------------------------

(deftest state-exception-is-wrapped-with-machine-context-test
  (let [cause (js/Error. "state exploded")
        machine
        (fx/machine
         :test/state-error
         :start
         (fn [_ctx]
           (throw cause)))
        error (thrown #(machine {}))
        data (ex-data error)]
    (is (= "State function threw an exception"
           (ex-message error)))
    (is (identical? cause (ex-cause error)))
    (is (= :test/state-error (:biff.fx/machine-name data)))
    (is (= :start (:biff.fx/state data)))
    (is (= [] (:biff.fx/trace data)))
    (is (= "state exploded" (:biff.fx/exception-message data)))
    (is (date? (:biff.fx/now data)))
    (is (integer? (:biff.fx/seed data)))))

(deftest later-state-exception-includes-prior-output-trace-test
  (let [machine
        (fx/machine
         :test/later-state-error
         :start
         (fn [_ctx]
           {:value 1
            :biff.fx/next :finish})
         :finish
         (fn [_ctx _previous]
           (throw (js/Error. "finish exploded"))))
        data (thrown-data #(machine {}))]
    (is (= :finish (:biff.fx/state data)))
    (is (= 1 (count (:biff.fx/trace data))))
    (is (= 1
           (get-in data [:biff.fx/trace 0 :value])))
    (is (= :finish
           (get-in data [:biff.fx/trace 0 :biff.fx/next])))))

(deftest handler-exception-is-wrapped-with-effect-context-test
  (let [cause (js/Error. "handler exploded")
        machine
        (fx/machine
         :test/handler-error
         :start
         (fn [_ctx]
           {:plain :kept
            :answer [:test/fail 1 2]}))
        error
        (thrown
         #(machine
           {:request-id "request-1"
            :biff.fx/handlers
            {:test/fail
             (fn [_ctx _left _right]
               (throw cause))}}))
        data (ex-data error)]
    (is (= "Handler function threw an exception"
           (ex-message error)))
    (is (identical? cause (ex-cause error)))
    (is (= :test/handler-error (:biff.fx/machine-name data)))
    (is (= :start (:biff.fx/state data)))
    (is (= :test/fail (:biff.fx/handler data)))
    (is (= [1 2] (:biff.fx/handler-args data)))
    (is (= "handler exploded" (:biff.fx/exception-message data)))
    (is (= :kept
           (get-in data [:biff.fx/output :plain])))
    (is (= [:test/fail 1 2]
           (get-in data [:biff.fx/output :answer])))
    (is (= [] (:biff.fx/trace data)))))

;; -----------------------------------------------------------------------------
;; Synchronous-only browser contract
;; -----------------------------------------------------------------------------

(deftest promise-handler-results-are-ordinary-host-values-test
  (let [promise (js/Promise.resolve :later)
        machine
        (fx/machine
         :test/promise-effect
         :start
         (fn [_ctx]
           {:result [:test/promise]}))]
    (is (identical?
         promise
         (:result
          (machine
           {:biff.fx/handlers
            {:test/promise
             (fn [_ctx]
               promise)}}))))))

(deftest promise-state-result-is-a-terminal-return-value-test
  (let [promise (js/Promise.resolve {:answer 42})
        machine
        (fx/machine
         :test/promise-state
         :start
         (fn [_ctx]
           promise))]
    (is (identical? promise (machine {})))))
