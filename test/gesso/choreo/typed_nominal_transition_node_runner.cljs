(ns gesso.choreo.typed-nominal-transition-node-runner
  "Portable Choreo Node assurance gate: execute every standard CLJC test
   namespace and reject any missing or duplicated test suite."
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
            [gesso.choreo.verify-test]
            [gesso.choreo.correspondence-test]
            [gesso.choreo.proof-test]
            [gesso.choreo.realization-test]
            [gesso.choreo.refinement-test]
            [gesso.choreo.reference-approval-test]
            [gesso.choreo.reference-chat-test]
            [gesso.choreo.reference-checkout-test]))

(def ^:private minimum-assertions 13370)
(def ^:private minimum-tests 582)

(def ^:private required-namespaces
  #{"gesso.choreo.typed-nominal-transition-boundary-test"
    "gesso.choreo.typed-nominal-machine-boundary-test"
    "gesso.choreo.machine-test"
    "gesso.choreo.identity-test"
    "gesso.choreo.type-test"
    "gesso.choreo.semantics-test"
    "gesso.choreo.core-test"
    "gesso.choreo.project-test"
    "gesso.choreo.knowledge-test"
    "gesso.choreo.verify-test"
    "gesso.choreo.correspondence-test"
    "gesso.choreo.proof-test"
    "gesso.choreo.realization-test"
    "gesso.choreo.refinement-test"
    "gesso.choreo.reference-approval-test"
    "gesso.choreo.reference-chat-test"
    "gesso.choreo.reference-checkout-test"})

(def ^:private visited-namespaces (atom []))

(derive ::reporter ::t/default)

(defmethod t/report [::reporter :begin-test-ns] [event]
  (swap! visited-namespaces conj (str (:ns event)))
  (when-let [reporter (get-method t/report [::t/default :begin-test-ns])]
    (reporter event)))

(defmethod t/report [::reporter :end-run-tests] [summary]
  (let [seen @visited-namespaces
        passed? (and (= required-namespaces (set seen))
                     (= (count required-namespaces) (count seen))
                     (>= (or (:test summary) 0) minimum-tests)
                     (>= (or (:pass summary) 0) minimum-assertions)
                     (zero? (or (:fail summary) 0))
                     (zero? (or (:error summary) 0)))]
    (println "GESSO_TYPED_NODE_TRANSITIONS" (pr-str summary))
    (when-not passed?
      (println "Missing, duplicated or diminished portable Choreo Node coverage.")
      (println "Observed test namespaces:" (pr-str seen)))
    (.exit js/process (if passed? 0 1))))

(defn main []
  (reset! visited-namespaces [])
  (println "GESSO_CHOREO_IDENTITY_PARITY"
           (pr-str (gesso.choreo.typed-nominal-transition-boundary-test/portable-identity-corpus)))
  (println "GESSO_CHOREO_ADMISSION_PARITY"
           (pr-str (gesso.choreo.typed-nominal-transition-boundary-test/portable-admission-corpus)))
  (println "GESSO_CHOREO_SUSPENSION_PARITY"
           (pr-str (gesso.choreo.typed-nominal-transition-boundary-test/portable-suspension-corpus)))
  (println "GESSO_CHOREO_ACTION_PARITY"
           (pr-str (gesso.choreo.typed-nominal-transition-boundary-test/portable-action-corpus)))
  (println "GESSO_CHOREO_WIRE_PARITY"
           (pr-str (gesso.choreo.identity-test/portable-wire-corpus)))
  (println "GESSO_CHOREO_BINDING_WIRE_PARITY"
           (pr-str (gesso.choreo.typed-nominal-transition-boundary-test/portable-binding-wire-corpus)))
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
               'gesso.choreo.verify-test
               'gesso.choreo.correspondence-test
               'gesso.choreo.proof-test
               'gesso.choreo.realization-test
               'gesso.choreo.refinement-test
               'gesso.choreo.reference-approval-test
               'gesso.choreo.reference-chat-test
               'gesso.choreo.reference-checkout-test))

(set! *main-cli-fn* main)
