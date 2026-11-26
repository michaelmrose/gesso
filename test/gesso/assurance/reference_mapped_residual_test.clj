(ns gesso.assurance.reference-mapped-residual-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-residual :as reference]
            [gesso.assurance.reference-mapped-writes-test :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-residual :as residual]))

(deftest derive-residual-assertions-from-closed-source
  (let [fields [:example/count :example/owner :example/outcome]
        source (-> @#'fixture/source
                   (assoc :guard {:readers (set fields)
                                  :expression [:and [:= [:read :example/count] [:literal 2]]
                                               [:= [:read :example/owner] [:literal nil]]
                                               [:= [:read :example/outcome] [:literal nil]]]})
                   (update :reads into fields))
        targets @#'fixture/targets
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :updated-at "t0" :request/location-id "l1"}
        docs {:gesso_requests [row] :gesso_assignments []}
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :example/claim :arguments {:request-id "r1"}}
        declaration {:document-layout {[:gesso_requests "r1"] [[:prestate :revision] [:state :example/count] [:prestate :updated-at]]}
                     :subject-checks [[:gesso_requests "r1" :request/location-id]]}
        declarations {:example/claim declaration}
        mapping (plan/check-target-mapping! source targets)
        derive #(reference/derive-assertions! source targets %1 [] binding %2)
        actual (residual/reconstruct! source mapping declaration binding docs)
        check #(reference/check-assertions! source targets docs [] binding declarations %)]
    (is (= declarations (residual/check-catalog! {:example/claim mapping}
                                                 [{:id :example/claim :construction source}] declarations)))
    (is (= :unsupported (:status (reference/check-assertions! @#'fixture/source targets docs [] binding declarations actual))))
    (is (= 5 (count actual)))
    (is (= actual (derive docs declarations)))
    (is (= :matched (:status (check actual))))
    (is (= :source-relative-residual-fragment (:scope (check actual))))
    (is (= :none (:authority (check actual))))
    (is (= :not-issued (:certificate (check actual))))
    (is (= :not-established (:provider-refinement (check actual))))
    (is (= [] (derive docs {:example/claim nil})))
    (is (= :matched (:status (reference/check-assertions! source targets docs [] binding {:example/claim nil} []))))
    (doseq [bad [{} {:example/claim false} {:example/claim {}}
                 {:example/claim (assoc declaration :extra true)}
                 {:example/claim (assoc declaration :document-layout {})}
                 {:example/claim (assoc-in declaration [:document-layout [:gesso_requests "r1"]] [1])}
                 {:example/claim (assoc declaration :subject-checks [1])}
                 {:example/claim (assoc-in declaration [:document-layout [:gesso_requests "r1"]] [[:state :example/count] [:prestate :amount]])}
                 {:example/claim (assoc declaration :subject-checks [[:gesso_requests "r1" :missing]])}
                 {:example/claim (assoc declaration :subject-checks [[:gesso_requests "r2" :revision]])}]]
      (is (thrown? clojure.lang.ExceptionInfo (derive docs bad))))
    (doseq [bad [(pop actual) (conj actual (first actual)) (vec (reverse actual))
                 (assoc-in actual [0 2] "other") (assoc-in actual [0 1] 0N)
                 (with-meta actual {:untrusted true}) (apply list actual)]]
      (let [r (check bad)]
        (is (= :mismatch (:status r)))
        (is (not (contains? r :scope)))))
    (let [changed (-> docs (assoc-in [:gesso_requests 0 :revision] 9) (assoc-in [:gesso_requests 0 :request/location-id] "l2"))]
      (is (= (residual/reconstruct! source mapping declaration binding changed) (derive changed declarations)))
      (is (not= actual (derive changed declarations))))))
