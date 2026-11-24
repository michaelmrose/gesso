(ns gesso.live.outbox.http-aleph-qualification-main
  "Fail-closed, explicit real-Aleph test entry point. The ordinary test
   runner's test-path discovery can silently execute zero tests for this
   opt-in integration root; no zero-test success is acceptable here."
  (:require [clojure.test :as t]))

(def ^:private namespaces-to-run
  ['gesso.live.outbox.http-aleph-integration-test
   'gesso.assurance.example.physical-single-node-aleph-integration-test])

(defn -main [& args]
  (when (seq args)
    (throw (ex-info "Unexpected Aleph qualification arguments"
                    {:args (vec args)})))
  ;; This require MUST fail if the real Aleph dependency is absent. Do not
  ;; substitute a mock or fallback listener for qualification.
  (doseq [namespace-to-run namespaces-to-run]
    (require namespace-to-run)
    (let [declared (count (filter (comp :test meta val)
                                  (ns-publics namespace-to-run)))
          minimum (if (= namespace-to-run (first namespaces-to-run)) 2 1)]
      (when (< declared minimum)
        (throw (ex-info "Aleph qualification has too few test cases"
                        {:declared declared :namespace namespace-to-run})))
      (let [{:keys [test fail error] :as result}
            (t/run-tests namespace-to-run)]
        (when (or (not= declared test)
                  (pos? fail)
                  (pos? error))
          (throw (ex-info "Real Aleph qualification did not pass"
                          (assoc result :declared declared
                                 :namespace namespace-to-run))))))))
