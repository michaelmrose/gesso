(ns gesso.choreo.typed-nominal-transition-node-runner
  "Focused Node/CLJS CI runner for identity, machine, semantics and projection.
   All included suites belong to Gesso's standard Choreo Node corpus."
  (:require [cljs.test :as t]
            [gesso.choreo.typed-nominal-transition-boundary-test]
            [gesso.choreo.typed-nominal-machine-boundary-test]
            [gesso.choreo.machine-test]
            [gesso.choreo.identity-test]
            [gesso.choreo.type-test]
            [gesso.choreo.semantics-test]
            [gesso.choreo.core-test]
            [gesso.choreo.project-test]))

(def ^:private minimum-assertions 8810)
(def ^:private minimum-tests 315)

(derive ::reporter ::t/default)

(defmethod t/report [::reporter :end-run-tests] [summary]
  (let [passed? (and (>= (or (:test summary) 0) minimum-tests)
                     (>= (or (:pass summary) 0) minimum-assertions)
                     (zero? (or (:fail summary) 0))
                     (zero? (or (:error summary) 0)))]
    (println "GESSO_TYPED_NODE_TRANSITIONS" (pr-str summary))
    (when-not passed?
      (println "Missing or diminished Node/CLJS identity, machine and projection coverage."))
    (.exit js/process (if passed? 0 1))))

(defn main []
  (t/run-tests (t/empty-env ::reporter)
               'gesso.choreo.typed-nominal-transition-boundary-test
               'gesso.choreo.typed-nominal-machine-boundary-test
               'gesso.choreo.machine-test
               'gesso.choreo.identity-test
               'gesso.choreo.type-test
               'gesso.choreo.semantics-test
               'gesso.choreo.core-test
               'gesso.choreo.project-test))

(set! *main-cli-fn* main)
