(ns gesso.assurance.reference-command-route-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.reference-mapped-submission :as submission]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]))

(def ^:private route
  {:route-id :request/claim :path "/operations/request/:request-id/claim"
   :method :post :operation :request/claim :required-transport :htmx
   :uri "/operations/request/r1/claim" :path-arguments {:request-id "r1"}
   :request-gate :locally-accepted :evidence :matched-canonical-htmx-handoff})

(defn- origin [id execution]
  {:principal (identity/principal "helper") :command-id (identity/command-id id)
   :execution-id (identity/execution-id execution) :operation :request/claim
   :arguments {:request-id "r1"} :origin :registered-server-invocation
   :authentication :conditional-on-trusted-principal-resolver :http-route route})

(deftest selected-route-observation-preserves-checked-bindings
  (let [origins [(origin "one" "e1") (origin "two" "e2") (origin "two" "e2")]
        steps (mapv (fn [tx o] {:spec {:binding (select-keys o resolution/binding-keys)}
                                :expectation {:tx-id tx}}) [1 2 3] origins)
        claims (mapv #(hash-map :command-origin %) origins)
        compare! #'submission/compare-command-origins!
        report (compare! steps claims)]
    (is (= :matched-supplied-command-bindings (:command-origin-correspondence report)))
    (is (= [route route route] (mapv :http-route (:selected-command-origins report))))
    (is (= :same-observed-execution (:abort-recovery-execution report)))
    (is (= :not-established (:command-authentication report)))
    (is (nil? (get-in (compare! steps (update-in claims [0 :command-origin] dissoc :http-route))
                      [:selected-command-origins 0 :http-route])))
    (doseq [bad [nil (dissoc route :route-id) (assoc route :extra true)
                 (with-meta route {:unexpected true})
                 (assoc route :route-id "claim") (assoc route :method :get)
                 (assoc route :operation :request/cancel) (assoc route :required-transport :sse)
                 (assoc route :path "/wrong/:request-id")
                 (assoc route :uri "/operations/request/r2/claim")
                 (assoc route :uri "/operations/request/r1/claim?x=1")
                 (assoc route :path-arguments {:request-id "r2"})
                 (assoc route :request-gate :authenticated) (assoc route :evidence :forged)]]
      (is (= :mapped-command-origin-mismatch
             (try (compare! steps (assoc-in claims [0 :command-origin :http-route] bad)) nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
    (let [describe #'recovery/command-authentication-premises
          entry {:scope {:publication-source-id "claim" :historical-cut {:payload-fingerprint "fixture"}}
                 :command-observation-ref "original-cut"
                 :judgment-subject {:catalog-id :fixture/claim :selected-operations [:accept :accept :refuse]}
                 :original-recovery (merge report {:selected-tx-ids [1 2 3]
                                                   :selected-commitments [:committed :aborted :committed]})}
          premise (describe entry)
          path [:original-recovery :selected-command-origins 0 :http-route]]
      (is (= route (get-in premise (into [:scope] (drop 1 path)))))
      (doseq [other [(update-in entry (pop path) dissoc :http-route)
                     (assoc-in entry (conj path :route-id) :another/claim)
                     (assoc-in entry (conj path :request-gate) :other)]]
        (let [changed (describe other)]
          (is (not-any? (:open-premise-fingerprints premise) (:open-premise-fingerprints changed)))
          (is (= :not-established (:command-authentication changed)))
          (is (= :not-performed (:premise-acceptance changed))))))))
