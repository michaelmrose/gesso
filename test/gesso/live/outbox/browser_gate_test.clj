(ns gesso.live.outbox.browser-gate-test
  "v2113: fail-closed browser-gate orchestration and reason classification.

   These are JVM unit tests and do NOT count as real Chromium convergence."
  (:require [clojure.test :as t :refer [deftest is]]
            [gesso.live.outbox.browser-gate :as gate]))

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error
         (:reason (ex-data error)))))

(deftest classify-browser-navigation-policy-and-other-failures-test
  (is (= :managed-navigation-block
         (gate/navigation-failure-reason
          (ex-info "Outer exception"
                   {}
                   (RuntimeException.
                    "net::ERR_BLOCKED_BY_ADMINISTRATOR at 127.0.0.1")))))
  (is (= :navigation-failed
         (gate/navigation-failure-reason
          (RuntimeException. "net::ERR_CONNECTION_REFUSED"))))
  (is (= :navigation-failed
         (gate/navigation-failure-reason
          (ex-info "Timeout" {:reason :timeout})))))

(deftest refused-environment-never-runs-browser-test-test
  (let [browser-called? (atom false)]
    (with-redefs [gate/preflight! (fn [] (throw (ex-info "Browser is blocked"
                                                     {:reason :managed-navigation-block})))
                  gate/run-browser! (fn [] (reset! browser-called? true))]
      (is (= :managed-navigation-block (reason #(gate/execute! :browser))))
      (is (false? @browser-called?)))))

(deftest ready-environment-still-requires-real-browser-test-pass-test
  (let [called (atom 0)]
    (with-redefs [gate/preflight! (fn [] {:status :environment-ready
                                          :browser-convergence-verified? false})
                  gate/run-browser! (fn [] (swap! called inc)
                                      {:test 1 :pass 20 :fail 0 :error 0})]
      (let [result (gate/execute! :browser)]
        (is (= :browser-convergence-passed (:status result)))
        (is (true? (:browser-convergence-verified? result)))
        (is (= 1 (:test (:results result))))
        (is (= 1 @called))))
    (with-redefs [gate/preflight! (fn [] {:status :environment-ready})
                  gate/run-browser! (fn [] (throw (ex-info "Browser test failed"
                                                         {:reason :assertion-failure})))]
      (is (= :assertion-failure (reason #(gate/execute! :browser)))))))

(deftest readiness-and-preflight-never-claim-browser-convergence-test
  (let [browser-called? (atom false)]
    (with-redefs [gate/preflight! (fn [] {:status :environment-ready
                                          :browser-convergence-verified? false})
                  gate/run-browser! (fn [] (reset! browser-called? true))
                  gate/run-readiness! (fn [] {:test 4 :pass 66 :fail 0 :error 0})]
      (let [preflight (gate/execute! :preflight)
            readiness (gate/execute! :readiness)]
        (is (= :environment-ready (:status preflight)))
        (is (false? (:browser-convergence-verified? preflight)))
        (is (= :http-readiness-passed (:status readiness)))
        (is (false? (:browser-convergence-verified? readiness)))
        (is (= 4 (get-in readiness [:results :test])))
        (is (false? @browser-called?))))
    (is (= :invalid-mode (reason #(gate/execute! :skip-browser))))))

(deftest zero-or-failing-browser-selection-cannot-be-success-test
  (with-redefs [gate/preflight! (fn [] {:status :environment-ready})
                gate/run-browser! (fn [] {:test 0 :pass 0 :fail 0 :error 0})]
    (is (= :assertion-failure (reason #(gate/execute! :browser)))))
  (with-redefs [gate/preflight! (fn [] {:status :environment-ready})
                gate/run-browser! (fn [] {:test 1 :pass 0 :fail 1 :error 0})]
    (is (= :assertion-failure (reason #(gate/execute! :browser)))))
  (with-redefs [gate/preflight! (fn [] {:status :unexpected})
                gate/run-browser! (fn [] (throw (AssertionError. "must not run")))]
    (is (= :browser-preflight-not-ready
           (reason #(gate/execute! :browser)))))
  (with-redefs [gate/run-readiness! (fn [] {:test 0 :pass 0 :fail 0 :error 0})]
    (is (= :assertion-failure (reason #(gate/execute! :readiness))))))


(deftest readiness-selection-requires-every-selected-case-test
  (let [observed (atom [])
        count-test! (fn [] (t/inc-report-counter :test)
                            (t/inc-report-counter :pass))]
    (with-redefs [t/test-vars (fn [vars]
                               (reset! observed (mapv #(symbol (str (ns-name (:ns (meta %))))
                                                             (str (:name (meta %)))) vars))
                               (doseq [_ vars] (count-test!)))]
      (is (= 11 (:test (gate/run-readiness!))))
      (is (= 11 (count (distinct @observed))))
      (is (every? (set @observed)
                  '[gesso.live.outbox.http-fragment-test/assembled-streams-share-capacity-and-use-original-model-policy
                    gesso.live.outbox.http-fragment-test/installed-model-command-recovers-submitted-transaction-without-replay-write
                    gesso.live.outbox.frontier-recovery-test/submitted-receipt-survives-cycle-interruption-and-rescan-test
                    gesso.assurance.example.physical-single-node-session-test/actual-jetty-requests-require-server-issued-session-and-csrf
                    gesso.assurance.example.physical-single-node-durable-live-test/managed-worker-delivers-protected-http-writes-without-manual-processing
                    gesso.assurance.example.physical-single-node-durable-live-test/managed-worker-scope-retirement-precedes-provider-close-on-exception
                    gesso.assurance.example.physical-single-node-durable-live-test/closed-application-reopens-persisted-reservation-and-recovers-pending-intent])))
    ;; Positive but incomplete test counts used to satisfy the gate.
    (with-redefs [t/test-vars (fn [vars]
                               (doseq [_ (butlast vars)] (count-test!)))]
      (is (= :assertion-failure (reason gate/run-readiness!)))
      (is (= :assertion-failure (reason gate/run-browser!))))))
