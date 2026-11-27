(ns gesso.assurance.reference-read-only-replay-argument-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-read-only-replay-argument :as replay]
            [gesso.assurance.reference-source-transition-argument :as committed]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.model.command-resolution :as resolution]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- fixture
  ([] (fixture (claim/catalog)))
  ([original]
   (let [source {:inductive-source {:catalog original} :target-map claim/target-map}
         declarations (claim/outcome-declarations)
         fp (:fingerprint (catalog/check-operation-set! original))
         cases (:cases (outcome/derive-cases! source declarations))]
     [source declarations fp cases])))

(defn- witness [helper branch]
  (let [other (if (= "helper-1" helper) "helper-2" "helper-1")
        owner (if (= :accept branch) helper other)
        binding-for (fn [h] {:command-id (identity/command-id h)
                             :principal (identity/principal h)
                             :operation :request/claim :arguments {:request-id "r1"}})
        accepted {:outcome :request/claimed :helper owner}
        rejected {:outcome :request/not-claimable :reason :already-claimed}
        records {owner (resolution/command-record (binding-for owner) accepted "t1")
                 (if (= owner helper) other helper)
                 (resolution/command-record (binding-for (if (= owner helper) other helper)) rejected "t2")}
        rows {:requests {"r1" {:xt/id "r1" :status :claimed :revision 1}}
              :request_assignments {"primary-r1" {:xt/id "primary-r1" :helper owner}}
              :memberships {"helper-1" {:xt/id "helper-1" :active true}
                            "helper-2" {:xt/id "helper-2" :active true}}
              :gesso_commands records}]
    {:before rows :after rows :binding (binding-for helper)
     :result {:status :replay :outcome (if (= :accept branch) accepted rejected)}
     :submissions [] :disclosure-permitted? true}))

(deftest all-original-replay-alternatives
  (let [args (fixture) report (apply replay/derive! args)
        writes (apply committed/derive! args)]
    (is (= :conditional-catalog-replay-derivation (:status report)) (pr-str report))
    (is (= (:outside-fragment writes) (set (keys (:alternatives report)))))
    (is (= 4 (count (:alternatives report))))
    (is (= :not-established (:writer-coverage report) (:universal-refinement report)))
    (is (= :not-performed (:proof-discharge report)))
    (is (contains? (:remaining-conditions report) :no-replay-attributed-mutations-or-submissions))
    (is (contains? (:remaining-conditions report) :faithful-single-cut-record-and-state-projection))
    (doseq [helper (keys claim/helpers) branch [:accept :refuse]]
      (let [id (claim/alternative-id helper (if (= branch :accept) :replay-accepted :replay-rejected))
            alternative (get-in report [:alternatives id])
            observed (apply replay/check-witness! (concat args [id (witness helper branch)]))]
        (is (= :conditional-read-only-transition-derivation (:status alternative)))
        (is (= (claim/alternative-id helper branch) (get-in alternative [:origin :alternative-id])))
        (is (= (nth args 2) (get-in alternative [:identity :catalog-fingerprint])))
        (is (= (:source-fact alternative) (:normalized-goal alternative)))
        (is (= (set (keys claim/state-types)) (:frame alternative) (set (keys (:frame-equations alternative)))))
        (is (= #{} (:writes alternative)))
        (is (= :conditional-selected-replay-correspondence (:status observed)) (pr-str observed))
        (is (= (:before observed) (:after observed)))
        (is (= (:identity alternative) (:identity observed)))))))

(deftest reject-unqualified-read-only-observations
  (let [args (fixture) id (claim/alternative-id "helper-1" :replay-accepted)
        w (witness "helper-1" :accept)
        run #(apply replay/check-witness! (concat args [id %]))]
    (doseq [bad [(assoc w :submissions [[:put-docs :requests {:xt/id "r1"}]])
                 (assoc w :disclosure-permitted? false)
                 (assoc w :result {:status :unavailable})
                 (assoc-in w [:result :outcome] {:outcome :request/not-claimable})
                 (assoc-in w [:binding :command-id] (identity/command-id "different"))
                 (assoc-in w [:binding :arguments] {:request-id "other"})
                 (assoc-in w [:binding :principal] (identity/principal "helper-2"))
                 (assoc-in w [:after :requests "r1" :revision] 2)
                 (assoc-in w [:after :requests "r1" :extra] true)
                 (update w :before dissoc :gesso_commands)
                 (witness "helper-1" :refuse)
                 (let [rows (assoc-in (:before w) [:requests "r1" :revision] "bad")]
                   (assoc w :before rows :after rows))
                 (let [record (get-in w [:before :gesso_commands "helper-1"])
                       duplicate (-> record (assoc :xt/id "duplicate")
                                     (assoc-in [:gesso.command/binding :command-id] (identity/command-id "duplicate")))
                       rows (assoc-in (:before w) [:gesso_commands "duplicate"] duplicate)]
                   (assoc w :before rows :after rows))]]
      (is (= :not-established (:status (run bad))) (pr-str bad)))
    (is (= :not-established (:status (apply replay/check-witness! (concat args [(claim/alternative-id "helper-1" :accept) w])))))
    (with-redefs [resolution/reconcile (constantly (:result w))]
      (is (= :replay-boundary-replaced (:reason (apply replay/derive! args)))))))

(deftest reject-ambiguous-catalog-replay-coverage
  (let [id (claim/alternative-id "helper-1" :replay-rejected)
        original (claim/catalog)
        altered (update original :operations
                        (fn [ops] (mapv (fn [op] (if (= id (:id op))
                                                   (assoc-in op [:construction :guard :expression 2 1] :accepted)
                                                   op)) ops)))
        args (fixture altered)]
    (is (= :incomplete-or-ambiguous-replay-coverage (:reason (apply replay/derive! args))))))
