(ns gesso.assurance.reference-recovery-projection-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- record [id principal operation arguments outcome]
  (resolution/command-record {:command-id (identity/command-id id)
                              :principal (identity/principal principal)
                              :operation operation :arguments arguments} outcome "fixture-time"))

(deftest genuine-claim-coordinate-decoding
  (let [source {:inductive-source {:catalog (claim/catalog)} :target-map claim/target-map}
        cases (:cases (outcome/derive-cases! source (claim/outcome-declarations)))
        rows {:requests {"r1" {:xt/id "r1" :status :claimed :revision 1}}
              :request_assignments {"primary-r1" {:xt/id "primary-r1" :helper "helper-1"}}
              :memberships {"helper-1" {:xt/id "helper-1" :active true}
                            "helper-2" {:xt/id "helper-2" :active false}}}
        accepted (record "a" "helper-1" :request/claim {:request-id "r1"}
                         {:outcome :request/claimed :helper "helper-1"})
        refused (record "b" "helper-2" :request/claim {:request-id "r1"}
                        {:outcome :request/not-claimable :reason :already-claimed})
        run #(projection/project! claim/target-map % cases)]
    (doseq [visible [rows (assoc rows :gesso_commands {"a" accepted})
                     (assoc rows :gesso_commands {"a" accepted "b" refused})
                     (dissoc rows :request_assignments)]]
      (is (= (poststate/project-visible! claim/target-map visible {:outcome-cases cases})
             (run visible))))
    (is (false? (:membership/two (run rows))))
    (is (nil? (:assignment/helper (run (dissoc rows :request_assignments)))))
    (is (nil? (:journal/one (run rows))))
    (doseq [bad [(assoc rows :gesso_commands {"wrong" accepted})
                 (assoc rows :gesso_commands {"a" (assoc accepted :gesso.command/status :rejected)})
                 (assoc rows :gesso_commands {"a" accepted "c" (record "c" "helper-1" :request/claim
                                                               {:request-id "r1"} {:outcome :request/claimed :helper "helper-1"})})
                 (assoc rows :gesso_commands {"a" (assoc-in accepted [:gesso.command/binding :arguments] {:request-id "other"})})
                 (assoc rows :gesso_commands {"a" (assoc accepted :gesso.command/outcome {:outcome :unknown})})
                 (assoc-in rows [:request_assignments "primary-r1"] nil)
                 (update-in rows [:memberships "helper-2"] dissoc :active)
                 (assoc-in rows [:requests "r1" :xt/id] "foreign")
                 (dissoc rows :requests)]]
      (is (thrown? clojure.lang.ExceptionInfo (run bad))))))

(deftest coordinate-shape-and-boundary-rejection
  (let [coords {:fixture/value [:document :fixture_rows "row" :some_value]}
        rows {:fixture_rows {"row" {:xt/id "row" :some-value nil}}}]
    (is (= {:fixture/value nil} (projection/project! coords rows {})))
    (doseq [coord [[:other :fixture_rows "row" :some_value]
                   [:document :fixture_rows "row" :some-value]
                   [:document :gesso_commands "row" :some_value]
                   [:journal-slot "helper" :unqualified]]]
      (is (thrown? clojure.lang.ExceptionInfo (projection/project! {:fixture/value coord} rows {}))))
    (with-redefs [resolution/require-record identity]
      (is (not (projection/boundaries-current?)))
      (is (thrown? clojure.lang.ExceptionInfo (projection/project! coords rows {}))))))
