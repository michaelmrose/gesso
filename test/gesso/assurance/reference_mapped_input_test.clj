(ns gesso.assurance.reference-mapped-input-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-input :as input]
            [gesso.assurance.reference-mapped-poststate-test :as fixture]
            [gesso.model.command-resolution :as resolution]))

(defn- example [selector binding value]
  (let [original fixture/source
        selectors {:example/next-count selector}
        source {:inductive-source {:catalog {:operations [{:id :example/claim :construction original}]}}
                :target-map fixture/targets :input-derivations {:example/claim selectors}}
        record (resolution/command-record binding {:status :claimed} "t1")
        spec {:inputs {:example/next-count value} :effects {[:document :gesso_requests "r1"] {}
                                                           [:document :gesso_assignments "a1"] {}}
              :binding binding :outcome {:status :claimed} :now "t1" :outcome-key :status}
        tx (fixture/tx 1 [[:put-docs :gesso_requests (assoc fixture/row :amount value)]
                          [:put-docs :gesso_assignments {:xt/id "a1" :owner "helper-a"}]
                          [:put-docs :gesso_commands record]])]
    [source original fixture/targets {:gesso_requests [fixture/row] :gesso_assignments []} [] tx spec {:selectors selectors}]))

(defn- check! [args] (apply input/compare-selected! args))

(deftest selector-evaluation-is-not-a-fingerprint-check
  (let [binding (assoc-in fixture/command-binding [:arguments :quantity] 3)
        args (example [:argument :quantity] binding 3)
        r (check! args)]
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched-source-relative-selectors (:input-derivation r)))
    (is (= :matched (:candidate-command-record r)))
    (is (= :not-established (:source-provenance r)))
    (is (= :not-established (:outcome-provenance r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:provider-refinement r)))
    (is (= :selected-input-valuation-mismatch (:reason (check! (example [:argument :quantity] binding 4)))))
    (is (= :missing-selected-input-argument (:reason (check! (example [:argument :quantity] fixture/command-binding 3)))))
    (is (= :matched (:status (check! (example [:state :example/count] binding 2)))))
    (is (= :selected-input-valuation-mismatch (:reason (check! (example [:state :example/count] binding 3)))))
    (is (= :selected-input-selector-mismatch (:reason (check! (assoc-in args [7 :selectors :example/next-count] [:state :example/count])))))
    (is (= :invalid-input-catalog-selection (:reason (check! (assoc-in args [0 :inductive-source :catalog :operations 0 :construction :frame] #{})))))
    (is (= :selected-input-target-map-mismatch (:reason (check! (assoc-in args [0 :target-map] {})))))
    (is (= :invalid-source-input-selectors (:reason (check! (update-in args [0 :input-derivations :example/claim] dissoc :example/next-count)))))
    (is (= :selected-input-command-record-mismatch (:reason (check! (assoc-in args [6 :outcome] {:status :other})))))
    (is (= :invalid-selected-command-record (:reason (check! (update-in args [5 :ops] pop)))))
    (is (= :invalid-selected-command-record (:reason (check! (update-in args [5 :ops] conj (peek (get-in args [5 :ops])))))))
    (is (not (contains? (check! (example [:argument :quantity] binding 4)) :input-derivation)))))

(deftest input-free-source-requires-no-invented-selectors
  (let [args (example [:state :example/count] fixture/command-binding 2)
        original (-> fixture/source (assoc :input-types {} :reads #{:example/owner})
                     (assoc-in [:replacements :example/count] [:literal 2]))
        args (-> args (assoc 1 original) (assoc-in [0 :inductive-source :catalog :operations 0 :construction] original)
                 (update 0 dissoc :input-derivations) (assoc-in [6 :inputs] {}) (assoc 7 {:selectors {}}))]
    (is (= :matched (:status (check! args))) (pr-str (check! args)))
    (is (= :selected-input-selector-mismatch (:reason (check! (assoc-in args [7 :selectors] {:example/fake [:principal]})))))))

(deftest principal-selector-and-malformed-catalog
  (let [original (-> fixture/source
                     (assoc :input-types {:example/actor {:type :string}}
                            :reads #{:example/actor :example/owner})
                     (assoc-in [:replacements :example/count] [:literal 3])
                     (assoc-in [:replacements :example/owner] [:read :example/actor]))
        selectors {:example/actor [:principal]}
        args (-> (example [:argument :quantity] fixture/command-binding 3)
                 (assoc 1 original)
                 (assoc-in [0 :inductive-source :catalog :operations 0 :construction] original)
                 (assoc-in [0 :input-derivations :example/claim] selectors)
                 (assoc-in [6 :inputs] {:example/actor "helper-a"})
                 (assoc 7 {:selectors selectors}))]
    (is (= :matched (:status (check! args))) (pr-str (check! args)))
    (is (= :selected-input-valuation-mismatch (:reason (check! (assoc-in args [6 :inputs :example/actor] "helper-b")))))
    (is (= :invalid-source-input-selectors (:reason (check! (assoc-in args [0 :input-derivations :example/claim :example/actor] 7)))))
    (is (= :invalid-input-catalog-selection (:reason (check! (update-in args [0 :inductive-source :catalog :operations] #(conj % (first %)))))))))
