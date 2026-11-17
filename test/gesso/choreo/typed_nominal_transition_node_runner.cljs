(ns gesso.choreo.typed-nominal-transition-node-runner
  "Focused Node/CLJS CI runner for the portable sparse-identity transition
   corpus. The standard node:choreo harness still discovers this test too."
  (:require [cljs.test :as t]
            [gesso.choreo.typed-nominal-transition-boundary-test]))

(def ^:private minimum-assertions 4988)

(derive ::reporter ::t/default)

(defmethod t/report [::reporter :end-run-tests] [summary]
  (let [passed? (and (= 1 (:test summary))
                     (>= (or (:pass summary) 0) minimum-assertions)
                     (zero? (or (:fail summary) 0))
                     (zero? (or (:error summary) 0)))]
    (println "GESSO_TYPED_NODE_TRANSITIONS" (pr-str summary))
    (when-not passed?
      (println "Missing or diminished Node/CLJS sparse-transition coverage."))
    (.exit js/process (if passed? 0 1))))

(defn main []
  (t/run-tests (t/empty-env ::reporter)
               'gesso.choreo.typed-nominal-transition-boundary-test))

(set! *main-cli-fn* main)
