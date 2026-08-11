(ns gesso.live.browser.fx-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.fx :as fx]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

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

(defn- thrown-message
  [f]
  (some-> (thrown f)
          ex-message))

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
;; Handler resolution
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
            :not-callable}))]

    (is (= ":biff.fx/get-handlers"
           (:label data)))

    (is (= :not-callable
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
;; Machine construction
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
           :not-callable))]

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
;; Per-state browser injections
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
;; State accumulation
;; -----------------------------------------------------------------------------

(deftest state-output-is-merged-over-original-context-for-next-state-test
  (let [seen
        (atom nil)

        machine
        (fx/machine
         :test/accumulation

         :start
         (fn [ctx]
           {:from-start
            (inc
             (:base ctx))

            :biff.fx/next
            :finish})

         :finish
         (fn [ctx]
           (reset! seen ctx)

           {:biff.fx/return
            {:base
             (:base ctx)

             :from-start
             (:from-start ctx)}}))]

    (is (= {:base 10
            :from-start 11}
           (machine
            {:base 10})))

    (is (= 10
           (:base @seen)))

    (is (= 11
           (:from-start @seen)))))

(deftest later-state-output-overrides-earlier-input-test
  (let [machine
        (fx/machine
         :test/override

         :start
         (fn [_ctx]
           {:value
            :start

            :biff.fx/next
            :middle})

         :middle
         (fn [_ctx]
           {:value
            :middle

            :biff.fx/next
            :finish})

         :finish
         (fn [ctx]
           {:biff.fx/return
            (:value ctx)}))]

    (is (= :middle
           (machine
            {:value :original})))))

(deftest nil-state-output-is-empty-contribution-test
  (let [machine
        (fx/machine
         :test/nil-output

         :start
         (fn [_ctx]
           nil))]

    (is (= {}
           (machine {})))))

(deftest ordinary-state-output-without-next-or-return-is-final-result-test
  (let [machine
        (fx/machine
         :test/plain-result

         :start
         (fn [ctx]
           {:answer
            (inc
             (:value ctx))}))]

    (is (= {:answer 42}
           (machine
            {:value 41})))))

;; -----------------------------------------------------------------------------
;; Sequential state results
;; -----------------------------------------------------------------------------

(deftest sequential-state-results-reduce-left-to-right-test
  (let [calls
        (atom [])

        machine
        (fx/machine
         :test/sequential

         :start
         (fn [_ctx]
           [{:a 1}
            nil
            {:b 2}
            {:a 3}]))]

    (is (= {:a 3
            :b 2}
           (machine {})))))

(deftest later-sequential-result-sees-earlier-output-in-effect-handler-test
  (let [handler-contexts
        (atom [])

        machine
        (fx/machine
         :test/sequential-handler-context

         :start
         (fn [_ctx]
           [{:base 10}
            {:answer
             [:test/add-from-output 5]}]))

        result
        (machine
         {:biff.fx/handlers
          {:test/add-from-output
           (fn [ctx value]
             (swap!
              handler-contexts
              conj
              ctx)

             (+ (:base ctx)
                value))}})]

    (is (= {:base 10
            :answer 15}
           result))

    (is (= 1
           (count @handler-contexts)))

    (is (= 10
           (:base
            (first
             @handler-contexts))))))

(deftest sequential-results-may-carry-next-test
  (let [machine
        (fx/machine
         :test/sequential-next

         :start
         (fn [_ctx]
           [{:a 1}
            {:b 2
             :biff.fx/next
             :finish}])

         :finish
         (fn [ctx]
           {:biff.fx/return
            [(:a ctx)
             (:b ctx)]}))]

    (is (= [1 2]
           (machine {})))))

;; -----------------------------------------------------------------------------
;; Effect descriptor execution
;; -----------------------------------------------------------------------------

(deftest registered-effect-descriptor-is-replaced-by-handler-result-test
  (let [calls
        (atom [])

        machine
        (fx/machine
         :test/effect

         :start
         (fn [_ctx]
           {:answer
            [:test/add 20 22]}))

        result
        (machine
         {:request-id "request-1"

          :biff.fx/handlers
          {:test/add
           (fn [ctx left right]
             (swap!
              calls
              conj
              {:ctx ctx
               :left left
               :right right})

             (+ left right))}})]

    (is (= {:answer 42}
           result))

    (is (= 1
           (count @calls)))

    (is (= 20
           (:left
            (first @calls))))

    (is (= 22
           (:right
            (first @calls))))

    (is (= "request-1"
           (get-in
            @calls
            [0 :ctx :request-id])))))

(deftest multiple-effect-descriptors-in-one-result-are-executed-test
  (let [machine
        (fx/machine
         :test/multiple-effects

         :start
         (fn [_ctx]
           {:left
            [:test/inc 1]

            :right
            [:test/inc 10]

            :plain
            :preserved}))]

    (is (= {:left 2
            :right 11
            :plain :preserved}
           (machine
            {:biff.fx/handlers
             {:test/inc
              (fn [_ctx value]
                (inc value))}})))))

(deftest unregistered-vector-is-ordinary-data-test
  (let [machine
        (fx/machine
         :test/unregistered-vector

         :start
         (fn [_ctx]
           {:ordinary
            [:not/an-effect 1 2]}))]

    (is (= {:ordinary
            [:not/an-effect 1 2]}
           (machine {})))))

(deftest effect-handler-receives-plain-output-from-same-result-test
  (let [seen
        (atom nil)

        machine
        (fx/machine
         :test/same-result-context

         :start
         (fn [_ctx]
           {:base 40

            :answer
            [:test/add 2]}))]

    (is (= {:base 40
            :answer 42}
           (machine
            {:biff.fx/handlers
             {:test/add
              (fn [ctx value]
                (reset! seen ctx)
                (+ (:base ctx)
                   value))}})))

    (is (= 40
           (:base @seen)))))

(deftest effect-handler-result-replaces-descriptor-at-original-key-test
  (let [result-value
        {:nested true}

        machine
        (fx/machine
         :test/effect-replacement

         :start
         (fn [_ctx]
           {:result
            [:test/return-map]}))]

    (is (= {:result result-value}
           (machine
            {:biff.fx/handlers
             {:test/return-map
              (fn [_ctx]
                result-value)}})))))

;; -----------------------------------------------------------------------------
;; Dynamic-handler execution
;; -----------------------------------------------------------------------------

(deftest machine-uses-dynamic-handler-provider-test
  (let [calls
        (atom 0)

        machine
        (fx/machine
         :test/dynamic-handler

         :start
         (fn [_ctx]
           {:answer
            [:test/value]}))]

    (is (= {:answer :dynamic}
           (machine
            {:biff.fx/get-handlers
             (fn []
               (swap! calls inc)

               {:test/value
                (fn [_ctx]
                  :dynamic)})})))

    (is (= 1
           @calls))))

(deftest dynamic-handler-overrides-explicit-handler-during-execution-test
  (let [machine
        (fx/machine
         :test/dynamic-precedence

         :start
         (fn [_ctx]
           {:answer
            [:test/value]}))]

    (is (= {:answer :dynamic}
           (machine
            {:biff.fx/handlers
             {:test/value
              (fn [_ctx]
                :explicit)}

             :biff.fx/get-handlers
             (fn []
               {:test/value
                (fn [_ctx]
                  :dynamic)})})))))

;; -----------------------------------------------------------------------------
;; :biff.fx/next and :biff.fx/return
;; -----------------------------------------------------------------------------

(deftest next-transitions-through-local-states-test
  (let [visited
        (atom [])

        machine
        (fx/machine
         :test/next

         :start
         (fn [_ctx]
           (swap!
            visited
            conj
            :start)

           {:value 1
            :biff.fx/next
            :middle})

         :middle
         (fn [ctx]
           (swap!
            visited
            conj
            :middle)

           {:value
            (inc
             (:value ctx))

            :biff.fx/next
            :finish})

         :finish
         (fn [ctx]
           (swap!
            visited
            conj
            :finish)

           {:biff.fx/return
            (:value ctx)}))]

    (is (= 2
           (machine {})))

    (is (= [:start
            :middle
            :finish]
           @visited))))

(deftest return-terminates-immediately-test
  (let [visited
        (atom [])

        machine
        (fx/machine
         :test/return

         :start
         (fn [_ctx]
           (swap!
            visited
            conj
            :start)

           {:biff.fx/return
            :done})

         :never
         (fn [_ctx]
           (swap!
            visited
            conj
            :never)

           {:biff.fx/return
            :wrong}))]

    (is (= :done
           (machine {})))

    (is (= [:start]
           @visited))))

(deftest return-key-may-return-nil-test
  (let [machine
        (fx/machine
         :test/nil-return

         :start
         (fn [_ctx]
           {:biff.fx/return
            nil}))]

    (is (nil?
         (machine {})))))

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

(deftest next-to-missing-state-is-rejected-test
  (let [machine
        (fx/machine
         :test/missing-next

         :start
         (fn [_ctx]
           {:biff.fx/next
            :missing}))

        data
        (thrown-data
         #(machine {}))]

    (is (= :missing
           (:biff.fx/state data)))

    (is (= :test/missing-next
           (:biff.fx/machine-name data)))

    (is (= [:start]
           (vec
            (:biff.fx/available-states data))))))

;; -----------------------------------------------------------------------------
;; Invalid state result shapes
;; -----------------------------------------------------------------------------

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
;; State exception wrapping
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

;; -----------------------------------------------------------------------------
;; Handler exception wrapping
;; -----------------------------------------------------------------------------

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
;; State-input versus handler-input semantics
;; -----------------------------------------------------------------------------

(deftest state-receives-original-context-plus-accumulated-state-input-test
  (let [seen
        (atom nil)

        machine
        (fx/machine
         :test/state-input

         :start
         (fn [_ctx]
           {:derived
            10

            :biff.fx/next
            :finish})

         :finish
         (fn [ctx]
           (reset! seen ctx)

           {:biff.fx/return
            :done}))]

    (machine
     {:original
      5})

    (is (= 5
           (:original @seen)))

    (is (= 10
           (:derived @seen)))))

(deftest handler-receives-original-context-plus-current-plain-output-test
  (let [seen
        (atom nil)

        machine
        (fx/machine
         :test/handler-input

         :start
         (fn [_ctx]
           {:plain
            10

            :effect
            [:test/observe]}))]

    (machine
     {:original
      5

      :biff.fx/handlers
      {:test/observe
       (fn [ctx]
         (reset! seen ctx)
         :done)}})

    (is (= 5
           (:original @seen)))

    (is (= 10
           (:plain @seen)))

    (is (not
         (contains?
          @seen
          :effect)))))

;; -----------------------------------------------------------------------------
;; A realistic small browser FX workflow
;; -----------------------------------------------------------------------------

(deftest biff-style-browser-fx-workflow-test
  (let [effects
        (atom [])

        machine
        (fx/machine
         :example/save

         :start
         (fn [ctx]
           {:validated
            [:example.fx/validate
             (:value ctx)]

            :biff.fx/next
            :save})

         :save
         (fn [ctx]
           {:saved
            [:example.fx/save
             (:validated ctx)]

            :biff.fx/next
            :finish})

         :finish
         (fn [ctx]
           {:biff.fx/return
            {:validated
             (:validated ctx)

             :saved
             (:saved ctx)}}))

        result
        (machine
         {:value
          41

          :request-id
          "request-1"

          :biff.fx/handlers
          {:example.fx/validate
           (fn [ctx value]
             (swap!
              effects
              conj
              [:validate
               (:request-id ctx)
               value])

             (inc value))

           :example.fx/save
           (fn [ctx value]
             (swap!
              effects
              conj
              [:save
               (:request-id ctx)
               value])

             {:id "saved-1"
              :value value})}})]

    (is (= {:validated 42
            :saved
            {:id "saved-1"
             :value 42}}
           result))

    (is (= [[:validate
             "request-1"
             41]

            [:save
             "request-1"
             42]]
           @effects))))

;; -----------------------------------------------------------------------------
;; Synchronous-only contract
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
