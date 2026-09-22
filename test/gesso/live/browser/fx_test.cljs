(ns gesso.live.browser.fx-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.fx :as fx]))

;; Browser-specific implementation tests only.
;; Cross-platform Biff semantic correspondence belongs in
;; gesso.live.browser.fx-conformance-test.

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
  (is (= :biff.fx/handlers
         fx/handlers-key))
  (is (= :biff.fx/get-handlers
         fx/get-handlers-key))
  (is (= :biff.fx/next
         fx/next-key))
  (is (= :biff.fx/return
         fx/return-key))
  (is (= :biff.fx/now
         fx/now-key))
  (is (= :biff.fx/seed
         fx/seed-key))
  (is (= :biff.fx/state
         fx/state-key))
  (is (= :biff.fx/machine-name
         fx/machine-name-key))
  (is (= :biff.fx/trace
         fx/trace-key)))

;; -----------------------------------------------------------------------------
;; Handler resolution API
;; -----------------------------------------------------------------------------

(deftest empty-handler-map-test
  (is (= {}
         (fx/handlers {}))))

(deftest explicit-handler-map-test
  (let [handler
        (fn [_ctx]
          :handled)]
    (is (= {:test/handler handler}
           (fx/handlers
            {:biff.fx/handlers
             {:test/handler handler}})))))

(deftest dynamic-handlers-override-explicit-handlers-test
  (let [explicit
        (fn [_ctx]
          :explicit)

        dynamic
        (fn [_ctx]
          :dynamic)

        extra
        (fn [_ctx]
          :extra)

        calls
        (atom 0)

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

    (is (= 1
           @calls))

    (is (identical?
         dynamic
         (:test/shared resolved)))

    (is (identical?
         explicit
         (:test/explicit-only resolved)))

    (is (identical?
         extra
         (:test/dynamic-only resolved)))))

(deftest nil-dynamic-handler-result-is-empty-contribution-test
  (let [explicit
        (fn [_ctx]
          :explicit)]
    (is (= {:test/handler explicit}
           (fx/handlers
            {:biff.fx/handlers
             {:test/handler explicit}

             :biff.fx/get-handlers
             (fn []
               nil)})))))

(deftest handler-resolution-validates-context-test
  (let [data
        (thrown-data
         #(fx/handlers
           [:not :a-map]))]

    (is (= "FX context"
           (:label data)))

    (is (= [:not :a-map]
           (:value data)))))

(deftest explicit-handlers-must-be-map-test
  (let [data
        (thrown-data
         #(fx/handlers
           {:biff.fx/handlers
            [:not :a-map]}))]

    (is (= "FX handlers"
           (:label data)))

    (is (= [:not :a-map]
           (:value data)))))

(deftest dynamic-handler-provider-must-be-callable-test
  (let [data
        (thrown-data
         #(fx/handlers
           {:biff.fx/get-handlers
            42}))]

    (is (= ":biff.fx/get-handlers"
           (:label data)))

    (is (= 42
           (:value data)))))

(deftest dynamic-handler-provider-must-return-map-or-nil-test
  (let [data
        (thrown-data
         #(fx/handlers
           {:biff.fx/get-handlers
            (fn []
              [:not :a-map])}))]

    (is (= ":biff.fx/get-handlers result"
           (:label data)))

    (is (= [:not :a-map]
           (:value data)))))

(deftest handler-ids-must-be-keywords-test
  (let [data
        (thrown-data
         #(fx/handlers
           {:biff.fx/handlers
            {"not-a-keyword"
             (fn [_ctx]
               nil)}}))]

    (is (= "not-a-keyword"
           (:biff.fx/handler data)))))

(deftest handlers-must-be-callable-test
  (let [data
        (thrown-data
         #(fx/handlers
           {:biff.fx/handlers
            {:test/not-callable
             42}}))]

    (is (= "FX handler"
           (:label data)))

    (is (= 42
           (:value data)))))

;; -----------------------------------------------------------------------------
;; Machine construction validation
;; -----------------------------------------------------------------------------

(deftest machine-requires-start-state-test
  (let [data
        (thrown-data
         #(fx/machine
           :test/no-start

           :finish
           (fn [_ctx]
             {})))]

    (is (= :test/no-start
           (:biff.fx/machine-name data)))

    (is (map?
         (:biff.fx/states data)))))

(deftest machine-state-ids-must-be-keywords-test
  (let [data
        (thrown-data
         #(apply
           fx/machine
           :test/bad-state-id
           ["start"
            (fn [_ctx]
              {})]))]

    (is (= :test/bad-state-id
           (:biff.fx/machine-name data)))))

(deftest machine-state-values-must-be-callable-test
  (let [data
        (thrown-data
         #(fx/machine
           :test/bad-state-value

           :start
           42))]

    (is (= :test/bad-state-value
           (:biff.fx/machine-name data)))))

;; -----------------------------------------------------------------------------
;; Raw-state inspection arity
;; -----------------------------------------------------------------------------

(deftest raw-state-arity-calls-exact-state-with-exact-context-test
  (let [seen
        (atom nil)

        machine
        (fx/machine
         :test/raw

         :start
         (fn [ctx]
           (reset! seen ctx)
           {:value :start})

         :other
         (fn [ctx]
           (reset! seen ctx)
           {:value :other}))

        ctx
        {:original true
         :biff.fx/handlers
         {:test/unused
          (fn [_ctx]
            :unused)}}]

    (is (= {:value :other}
           (machine
            ctx
            :other)))

    (is (= ctx
           @seen))))

(deftest raw-state-arity-does-not-inject-time-or-seed-test
  (let [machine
        (fx/machine
         :test/raw-no-injection

         :start
         (fn [ctx]
           {:has-now?
            (contains? ctx
                       :biff.fx/now)

            :has-seed?
            (contains? ctx
                       :biff.fx/seed)}))]

    (is (= {:has-now? false
            :has-seed? false}
           (machine
            {}
            :start)))))

(deftest raw-state-arity-does-not-execute-effect-descriptors-test
  (let [handler-calls
        (atom 0)

        machine
        (fx/machine
         :test/raw-effect

         :start
         (fn [_ctx]
           {:answer
            [:test/effect 41]}))

        ctx
        {:biff.fx/handlers
         {:test/effect
          (fn [_ctx value]
            (swap! handler-calls inc)
            (inc value))}}]

    (is (= {:answer
            [:test/effect 41]}
           (machine
            ctx
            :start)))

    (is (= 0
           @handler-calls))))

(deftest raw-state-arity-rejects-unknown-state-test
  (let [machine
        (fx/machine
         :test/raw-missing

         :start
         (fn [_ctx]
           {}))

        data
        (thrown-data
         #(machine
           {}
           :missing))]

    (is (= :missing
           (:biff.fx/state data)))

    (is (= :test/raw-missing
           (:biff.fx/machine-name data)))

    (is (= [:start]
           (vec
            (:biff.fx/available-states data))))))

;; -----------------------------------------------------------------------------
;; Browser-only per-state injections
;; -----------------------------------------------------------------------------

(deftest normal-machine-injects-now-and-seed-test
  (let [seen
        (atom nil)

        machine
        (fx/machine
         :test/injections

         :start
         (fn [ctx]
           (reset!
            seen
            {:now
             (:biff.fx/now ctx)

             :seed
             (:biff.fx/seed ctx)})

           {:biff.fx/return
            :done}))]

    (is (= :done
           (machine {})))

    (is (date?
         (:now @seen)))

    (is (integer?
         (:seed @seen)))

    (is (<= 0
            (:seed @seen)))

    (is (< (:seed @seen)
           2147483647))))

(deftest injection-overrides-user-supplied-now-and-seed-test
  (let [seen
        (atom nil)

        user-now
        :fake-now

        user-seed
        :fake-seed

        machine
        (fx/machine
         :test/injection-precedence

         :start
         (fn [ctx]
           (reset!
            seen
            {:now
             (:biff.fx/now ctx)

             :seed
             (:biff.fx/seed ctx)})

           {:biff.fx/return
            :done}))]

    (machine
     {:biff.fx/now
      user-now

      :biff.fx/seed
      user-seed})

    (is (date?
         (:now @seen)))

    (is (not=
         user-now
         (:now @seen)))

    (is (integer?
         (:seed @seen)))

    (is (not=
         user-seed
         (:seed @seen)))))

(deftest each-state-receives-fresh-injected-values-test
  (let [seen
        (atom [])

        machine
        (fx/machine
         :test/per-state-injection

         :start
         (fn [ctx]
           (swap!
            seen
            conj
            [(:biff.fx/now ctx)
             (:biff.fx/seed ctx)])

           {:biff.fx/next
            :finish})

         :finish
         (fn [ctx]
           (swap!
            seen
            conj
            [(:biff.fx/now ctx)
             (:biff.fx/seed ctx)])

           {:biff.fx/return
            :done}))]

    (is (= :done
           (machine {})))

    (is (= 2
           (count @seen)))

    (is (every?
         (fn [[now seed]]
           (and
            (date? now)
            (integer? seed)))
         @seen))))

;; -----------------------------------------------------------------------------
;; Browser validation diagnostics
;; -----------------------------------------------------------------------------

(deftest next-and-return-together-are-rejected-test
  (let [machine
        (fx/machine
         :test/ambiguous-control

         :start
         (fn [_ctx]
           {:biff.fx/next
            :finish

            :biff.fx/return
            :done})

         :finish
         (fn [_ctx]
           {:biff.fx/return
            :finish}))

        data
        (thrown-data
         #(machine {}))]

    (is (= :start
           (:biff.fx/state data)))

    (is (= :test/ambiguous-control
           (:biff.fx/machine-name data)))

    (is (= :finish
           (get-in
            data
            [:biff.fx/output
             :biff.fx/next])))

    (is (= :done
           (get-in
            data
            [:biff.fx/output
             :biff.fx/return])))))

(deftest state-must-return-map-nil-or-sequence-of-map-nil-test
  (doseq [result
          [42
           "nope"
           :keyword
           [[:not :a-map]]
           [{:ok true}
            42]]]

    (let [machine
          (fx/machine
           :test/invalid-state-result

           :start
           (fn [_ctx]
             result))

          data
          (thrown-data
           #(machine {}))]

      (is (= :test/invalid-state-result
             (:biff.fx/machine-name data)))

      (is (= :start
             (:biff.fx/state data)))

      (is (= result
             (:biff.fx/result data))))))

;; -----------------------------------------------------------------------------
;; Exception wrapping and diagnostics
;; -----------------------------------------------------------------------------

(deftest state-exception-is-wrapped-with-machine-context-test
  (let [cause
        (js/Error.
         "state exploded")

        machine
        (fx/machine
         :test/state-error

         :start
         (fn [_ctx]
           (throw cause)))

        error
        (thrown
         #(machine {}))

        data
        (ex-data error)]

    (is (= "FX state function threw an exception."
           (ex-message error)))

    (is (identical?
         cause
         (ex-cause error)))

    (is (= :test/state-error
           (:biff.fx/machine-name data)))

    (is (= :start
           (:biff.fx/state data)))

    (is (= []
           (:biff.fx/trace data)))

    (is (= "state exploded"
           (:biff.fx/exception-message data)))

    (is (date?
         (:biff.fx/now data)))

    (is (integer?
         (:biff.fx/seed data)))))

(deftest later-state-exception-includes-prior-trace-test
  (let [machine
        (fx/machine
         :test/later-state-error

         :start
         (fn [_ctx]
           {:value 1
            :biff.fx/next
            :finish})

         :finish
         (fn [_ctx]
           (throw
            (js/Error.
             "finish exploded"))))

        data
        (thrown-data
         #(machine {}))]

    (is (= :finish
           (:biff.fx/state data)))

    (is (= 1
           (count
            (:biff.fx/trace data))))

    (is (= 1
           (get-in
            data
            [:biff.fx/trace
             0
             :value])))

    (is (= :finish
           (get-in
            data
            [:biff.fx/trace
             0
             :biff.fx/next])))))

(deftest handler-exception-is-wrapped-with-effect-context-test
  (let [cause
        (js/Error.
         "handler exploded")

        machine
        (fx/machine
         :test/handler-error

         :start
         (fn [_ctx]
           {:plain
            :kept

            :answer
            [:test/fail 1 2]}))

        error
        (thrown
         #(machine
           {:request-id
            "request-1"

            :biff.fx/handlers
            {:test/fail
             (fn [_ctx _left _right]
               (throw cause))}}))

        data
        (ex-data error)]

    (is (= "FX handler function threw an exception."
           (ex-message error)))

    (is (identical?
         cause
         (ex-cause error)))

    (is (= :test/handler-error
           (:biff.fx/machine-name data)))

    (is (= :start
           (:biff.fx/state data)))

    (is (= :test/fail
           (:biff.fx/handler data)))

    (is (= [1 2]
           (:biff.fx/handler-args data)))

    (is (= "handler exploded"
           (:biff.fx/exception-message data)))

    (is (= :kept
           (get-in
            data
            [:biff.fx/output
             :plain])))

    (is (not
         (contains?
          (:biff.fx/output data)
          :answer)))

    (is (= []
           (:biff.fx/trace data)))))

;; -----------------------------------------------------------------------------
;; Synchronous-only browser contract
;; -----------------------------------------------------------------------------

(deftest promise-results-are-not-special-cased-test
  (let [promise
        (js/Promise.resolve
         :later)

        machine
        (fx/machine
         :test/promise-is-data

         :start
         (fn [_ctx]
           {:result
            [:test/promise]}))]

    (is (identical?
         promise
         (:result
          (machine
           {:biff.fx/handlers
            {:test/promise
             (fn [_ctx]
               promise)}}))))))

(deftest machine-does-not-await-promise-state-result-test
  (let [promise
        (js/Promise.resolve
         {:answer 42})

        machine
        (fx/machine
         :test/promise-state

         :start
         (fn [_ctx]
           promise))

        data
        (thrown-data
         #(machine {}))]

    (is (= promise
           (:biff.fx/result data)))

    (is (= :start
           (:biff.fx/state data)))

    (is (= :test/promise-state
           (:biff.fx/machine-name data)))))
