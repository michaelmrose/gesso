(ns gesso.live.dsl-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.dsl :as dsl]
            [clojure.walk :as walk]))

(deftest defsynced-expansion-test
  (testing "defsynced generates the correct SQL string and value helper"
    (let [expansion (macroexpand-1
                     '(dsl/defsynced counter
                        {:path [:demo_counters "global-id" :demo/value]
                         :default 10}))]

      (is (= 'do (first expansion)))

      ;; Verify the generated SQL string is fully formed
      (let [defn-form (second expansion)
            ;; Navigating the backticked structure to find our SQL string
            sql-str   (some #(when (and (string? %) (clojure.string/includes? % "FROM demo_counters")) %)
                            (flatten expansion))]
        (is (not (nil? sql-str)) "SQL string should contain the table name"))

      ;; Verify the default value is baked in
      (is (some #(= 10 %) (flatten expansion))))))

(deftest defoperation-expansion-test
  (testing "defoperation rewrites swap! into put-and-publish!"
    ;; Manually seed the registry for the test environment
    (swap! dsl/!registry assoc "gesso.live.dsl-test/counter"
           {:path [:demo_counters "global-id" :demo/value]
            :val-fn 'counter-value})

    (let [expansion (walk/macroexpand-all
                     '(dsl/defoperation increment! [ctx]
                        (swap! counter inc)))]

      ;; Verify the inner body was rewritten from swap! to the write utility
      (let [flat-body (flatten expansion)]
        (is (some #(= 'gesso.live.consistency.xtdb/put-and-publish! %) flat-body)
            "Should expand to a put-and-publish! call")
        (is (some #(= :demo_counters %) flat-body)
            "Should preserve the table keyword")))))
