(ns gesso.choreo.typed-nominal-transition-node-runner
  "Focused Node/CLJS CI runner for portable Choreo machine and assurance behavior.
   All included suites belong to Gesso's standard Choreo Node corpus."
  (:require [cljs.test :as t]
            [gesso.choreo.typed-nominal-transition-boundary-test]
            [gesso.choreo.typed-nominal-machine-boundary-test]
            [gesso.choreo.machine-test]
            [gesso.choreo.identity-test]
            [gesso.choreo.type-test]
            [gesso.choreo.semantics-test]
            [gesso.choreo.core-test]
            [gesso.choreo.project-test]
            [gesso.choreo.knowledge-test]
            [gesso.choreo.verify-test]))

(def ^:private minimum-assertions 9152)
(def ^:private minimum-tests 406)

(derive ::reporter ::t/default)

(defmethod t/report [::reporter :end-run-tests] [summary]
  (let [passed? (and (>= (or (:test summary) 0) minimum-tests)
                     (>= (or (:pass summary) 0) minimum-assertions)
                     (zero? (or (:fail summary) 0))
                     (zero? (or (:error summary) 0)))]
    (println "GESSO_TYPED_NODE_TRANSITIONS" (pr-str summary))
    (when-not passed?
      (println "Missing or diminished Node/CLJS identity, machine and assurance coverage."))
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
               'gesso.choreo.project-test
               'gesso.choreo.knowledge-test
               'gesso.choreo.verify-test))

(set! *main-cli-fn* main)
