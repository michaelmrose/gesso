(ns gesso.assurance.reference-mapped-poststate-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution])
  (:import (java.time Instant)))

(def source
  {:subject {:kind :operation :id :example/claim :interface {:operation :example/claim}}
   :state-types {:example/count {:type :integer} :example/enabled {:type :boolean}
                 :example/owner {:type :string :nullable? true}
                 :example/outcome {:type :keyword :nullable? true}}
   :input-types {:example/next-count {:type :integer}}
   :guard {:readers #{:example/owner} :expression [:= [:read :example/owner] [:literal nil]]}
   :reads #{:example/next-count :example/owner}
   :writes #{:example/count :example/owner :example/outcome}
   :replacements {:example/count [:read :example/next-count]
                  :example/owner [:literal "helper-a"] :example/outcome [:literal :claimed]}
   :frame #{:example/enabled}})

(def targets
  {:example/count [:document :gesso_requests "r1" :amount]
   :example/enabled [:document :gesso_requests "r1" :enabled]
   :example/owner [:nullable-document :gesso_assignments "a1" :owner]
   :example/outcome [:journal-slot "helper-a" :example/claim]})

(def spec {:inputs {:example/next-count 3} :outcome-key :status})
(def row {:xt/id "r1" :amount 2 :enabled true})
(def command-binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
              :operation :example/claim :arguments {:request-id "r1"}})
(def record (resolution/command-record command-binding {:status :claimed} "t1"))
(def puts [[:put-docs :gesso_requests (assoc row :amount 3)]
           [:put-docs :gesso_assignments {:xt/id "a1" :owner "helper-a"}]
           [:put-docs :gesso_commands record]])
(defn tx [n ops]
  {:tx-id n :system-time (.plusSeconds (Instant/parse "2026-01-01T00:00:00Z") n) :ops ops})

(def initial (tx 0 [[:put-docs :gesso_requests row]]))
(def selected (tx 1 puts))
(def later (tx 2 [[:put-docs :gesso_requests (assoc row :amount 9 :enabled false)]]))

(defn check! [history expectation s]
  (poststate/compare-selected! history expectation source targets s))

(deftest selected-abstract-relation-uses-actual-replayed-rows
  (let [history [initial selected later]
        matched (check! history selected spec)]
    (is (= :matched (:status matched)) (pr-str matched))
    (is (= :matched-retained-projection (:abstract-transition matched)))
    (is (= :not-established (:provider-refinement matched)))
    (is (= :none (:authority matched)))
    (is (= :not-issued (:certificate matched)))
    (is (= matched (check! [initial selected] selected spec)))
    (is (= :mismatch (:status (check! 42 selected spec))))
    (is (= :mismatch (:status (check! (with-meta history {:untrusted true}) selected spec))))
    (is (= :selected-abstract-transaction-mismatch (:reason (check! history (assoc selected :ops []) spec))))
    (is (= :selected-abstract-transaction-mismatch (:reason (check! (conj history selected) selected spec))))
    (is (= :mismatch (:status (check! history selected (assoc-in spec [:inputs :example/next-count] 4)))))
    (let [cases {["helper-a" :example/claim {:request-id "r1"} {:status :claimed}] :claimed}
          s (-> spec (dissoc :outcome-key) (assoc :outcome-cases cases))]
      (is (= :matched (:status (check! history selected s))))
      (is (= :mismatch (:status (check! history selected (assoc s :outcome-cases (zipmap (keys cases) [:wrong])))))))))

(deftest invalid-committed-projections-and-abort-fail-closed
  (doseq [[ops expected-status] [[(assoc-in puts [0 2 :amount] 4) :mismatch]
                                 [(assoc-in puts [0 2 :enabled] false) :mismatch]
                                 [(update-in puts [0 2] dissoc :enabled) :unsupported]
                                 [(vec (remove #(= :gesso_assignments (second %)) puts)) :mismatch]
                                 [(vec (remove #(= :gesso_commands (second %)) puts)) :mismatch]
                                 [(conj puts [:put-docs :gesso_commands
                                              (resolution/command-record (assoc command-binding :command-id (identity/command-id "c2")) {:status :claimed} "t1")]) :unsupported]
                                 [(into [["ASSERT FALSE"]] puts) :mismatch]]]
    (let [attempt (tx 1 ops)
          result (check! [initial attempt] attempt spec)]
      (is (= expected-status (:status result)) (pr-str result))
      (is (not (contains? result :abstract-transition)))))
  (let [aborted (tx 1 (into [["ASSERT FALSE"]] puts))
        retry (tx 2 puts)]
    (is (= :selected-abstract-transaction-not-committed (:reason (check! [initial aborted retry] aborted spec))))
    (is (= :matched (:status (check! [initial aborted retry] retry spec))))))
