(ns gesso.fx-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.fx :as fx])
  (:import [java.time Instant]
           [java.util UUID]))

(deftest machine-runs-effects-across-transitions
  (let [seen
        (atom [])

        machine
        (fx/machine
         ::transition-test

         :start
         (fn [{:keys [from-ctx]}]
           [{:prefix from-ctx}
            {:combined     [:test/concat "-effect"]
             :biff.fx/next :finish}])

         :finish
         (fn [{:keys [prefix combined] :as ctx}]
           {:biff.fx/return
            {:prefix   prefix
             :combined combined
             :now?     (instance? Instant (:biff.fx/now ctx))
             :seed?    (integer? (:biff.fx/seed ctx))}}))]
    (is (= {:prefix   "ctx"
            :combined "ctx-effect"
            :now?     true
            :seed?    true}
           (machine
            {:from-ctx "ctx"

             :biff.fx/handlers
             {:test/concat
              (fn [ctx suffix]
                (swap! seen conj
                       (select-keys ctx
                                    [:from-ctx :prefix]))
                (str (:prefix ctx) suffix))}})))

    (is (= [{:from-ctx "ctx"
             :prefix   "ctx"}]
           @seen))))

(deftest machine-two-arity-runs-raw-state-function
  (let [machine
        (fx/machine
         ::raw-state-test

         :start
         (fn [{:keys [value]}]
           {:effect [:test/raw value]}))]
    (is (= {:effect [:test/raw 42]}
           (machine {:value 42}
                    :start)))))

(deftest machine-prefers-get-handlers-over-module-and-ctx-handlers
  (let [machine
        (fx/machine
         ::handler-precedence-test

         :start
         (fn [_]
           {:response
            [:biff.fx/http
             {:url "https://example.com"}]}))

        modules
        (atom
         [{:biff.fx/handlers
           {:biff.fx/http
            (fn [_ctx _request]
              :from-module-handlers)}}])]
    (is (= {:response :from-get-handlers}
           (machine
            {:biff/modules modules

             :biff.fx/handlers
             {:biff.fx/http
              (fn [_ctx _request]
                :from-ctx-handlers)}

             :biff.fx/get-handlers
             (fn []
               {:biff.fx/http
                (fn [_ctx _request]
                  :from-get-handlers)})})))))

(deftest module-handlers-override-ctx-handlers
  (let [machine
        (fx/machine
         ::module-handler-precedence-test

         :start
         (fn [_]
           {:response [:test/handler]}))

        modules
        (atom
         [{:biff.fx/handlers
           {:test/handler
            (fn [_ctx]
              :from-module-handlers)}}])]
    (is (= {:response :from-module-handlers}
           (machine
            {:biff/modules modules

             :biff.fx/handlers
             {:test/handler
              (fn [_ctx]
                :from-ctx-handlers)}})))))

(deftest machine-wraps-handler-errors-with-context
  (let [machine
        (fx/machine
         ::handler-error-test

         :start
         (fn [_]
           {:payload  (apply str (repeat 600 "x"))
            :response [:test/fail "boom"]}))

        ex
        (try
          (machine
           {:biff.fx/handlers
            {:test/fail
             (fn [_ctx message]
               (throw
                (ex-info message {})))}})
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (= "Handler function threw an exception"
           (ex-message ex)))

    (is (= ::handler-error-test
           (:biff.fx/machine-name
            (ex-data ex))))

    (is (= :start
           (:biff.fx/state
            (ex-data ex))))

    (is (= []
           (:biff.fx/trace
            (ex-data ex))))

    (is (= ["boom"]
           (:biff.fx/handler-args
            (ex-data ex))))

    (is (= 500
           (count
            (get-in
             (ex-data ex)
             [:biff.fx/output :payload]))))

    (is (str/ends-with?
         (get-in
          (ex-data ex)
          [:biff.fx/output :payload])
         "…"))))

(deftest machine-wraps-state-function-errors-with-context
  (let [machine
        (fx/machine
         ::state-error-test

         :start
         (fn [_]
           (throw
            (ex-info "boom" {}))))

        ex
        (try
          (machine {})
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (= "State function threw an exception"
           (ex-message ex)))

    (is (= ::state-error-test
           (:biff.fx/machine-name
            (ex-data ex))))

    (is (= :start
           (:biff.fx/state
            (ex-data ex))))

    (is (= []
           (:biff.fx/trace
            (ex-data ex))))

    (is (instance?
         Instant
         (:biff.fx/now
          (ex-data ex))))

    (is (integer?
         (:biff.fx/seed
          (ex-data ex))))))

(deftest machine-rejects-invalid-states
  (let [machine
        (fx/machine
         ::invalid-state-test

         :start
         (fn [_]
           {:biff.fx/next :missing}))

        ex
        (try
          (machine {})
          (catch clojure.lang.ExceptionInfo e
            e))]
    (is (= "Invalid state"
           (ex-message ex)))

    (is (= ::invalid-state-test
           (:biff.fx/machine-name
            (ex-data ex))))

    (is (= :missing
           (:biff.fx/state
            (ex-data ex))))

    (is (= #{:start}
           (set
            (:biff.fx/available-states
             (ex-data ex)))))))

(deftest machine-rejects-next-and-return-together
  (let [machine
        (fx/machine
         ::next-and-return-test

         :start
         (fn [_]
           {:biff.fx/next   :finish
            :biff.fx/return :done})

         :finish
         (fn [_]
           {:biff.fx/return :finished}))]
    (is (thrown-with-msg?
         AssertionError
         #"can't set :biff.fx/next and :biff.fx/return"
         (machine {})))))

(deftest module-contributes-fx-schema
  (is (= fx/schema
         (:schema
          (fx/module)))))

(deftest machine-collects-handlers-from-current-biff-1-modules
  (let [modules-var
        (atom
         [{:biff.fx/handlers
           {:test/a
            (fn [_ctx]
              :a)

            :test/shared
            (fn [_ctx]
              :first)}}

          {:biff.fx/handlers
           {:test/b
            (fn [_ctx]
              :b)

            :test/shared
            (fn [_ctx]
              :second)}}])

        machine
        (fx/machine
         ::module-handlers-test

         :start
         (fn [_]
           {:a      [:test/a]
            :b      [:test/b]
            :shared [:test/shared]}))

        ctx
        {:biff/modules modules-var}]
    (is (= {:a      :a
            :b      :b
            :shared :second}
           (machine ctx)))

    (swap!
     modules-var
     conj
     {:biff.fx/handlers
      {:test/c
       (fn [_ctx]
         :c)

       :test/shared
       (fn [_ctx]
         :third)}})

    (let [updated-machine
          (fx/machine
           ::updated-module-handlers-test

           :start
           (fn [_]
             {:a      [:test/a]
              :b      [:test/b]
              :c      [:test/c]
              :shared [:test/shared]}))]
      (is (= {:a      :a
              :b      :b
              :c      :c
              :shared :third}
             (updated-machine ctx))))))

(deftest machine-accepts-a-direct-module-vector
  (let [machine
        (fx/machine
         ::direct-modules-test

         :start
         (fn [_]
           {:response [:test/direct]}))

        ctx
        {:biff/modules
         [{:biff.fx/handlers
           {:test/direct
            (fn [_ctx]
              :direct)}}]}]
    (is (= {:response :direct}
           (machine ctx)))))

(deftest machine-handlers-receive-prior-output
  (let [seen
        (atom nil)

        machine
        (fx/machine
         ::handler-output-test

         :start
         (fn [_]
           [{:first-value 10}
            {:second-value
             [:test/add-to-first 5]}]))]
    (is (= {:first-value  10
            :second-value 15}
           (machine
            {:biff.fx/handlers
             {:test/add-to-first
              (fn [ctx amount]
                (reset! seen ctx)
                (+ (:first-value ctx)
                   amount))}})))

    (is (= 10
           (:first-value @seen)))))

(deftest uuid-is-deterministic-and-rfc-compatible
  (let [[uuid-a next-a] (fx/uuid 42)
        [uuid-b next-b] (fx/uuid 42)
        [uuid-c _]      (fx/uuid 43)]
    (is (instance? UUID uuid-a))
    (is (= uuid-a uuid-b))
    (is (= next-a next-b))
    (is (not= uuid-a uuid-c))
    (is (= 4 (.version uuid-a)))
    (is (= 2 (.variant uuid-a)))))

(deftest uuid4-is-deterministic-and-rfc-compatible
  (let [[uuid-a next-a] (fx/uuid4 42)
        [uuid-b next-b] (fx/uuid4 42)
        [uuid-c _]      (fx/uuid4 43)]
    (is (instance? UUID uuid-a))
    (is (= uuid-a uuid-b))
    (is (= next-a next-b))
    (is (not= uuid-a uuid-c))
    (is (= 4 (.version uuid-a)))
    (is (= 2 (.variant uuid-a)))))

(deftest uuid7-is-deterministic-and-rfc-compatible
  (let [instant         (Instant/parse
                         "2024-01-02T03:04:05Z")
        [uuid-a next-a] (fx/uuid7 42 instant)
        [uuid-b next-b] (fx/uuid7 42 instant)
        [uuid-c _]      (fx/uuid7 43 instant)]
    (is (instance? UUID uuid-a))
    (is (= uuid-a uuid-b))
    (is (= next-a next-b))
    (is (not= uuid-a uuid-c))
    (is (= 7 (.version uuid-a)))
    (is (= 2 (.variant uuid-a)))))
