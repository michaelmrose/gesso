(ns gesso.assurance.reference-source-write-argument-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-source-write-argument :as argument]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-catalog-symbolic-write-equations
  (let [original (claim/catalog)
        source {:inductive-source {:catalog original} :target-map claim/target-map}
        declarations (claim/outcome-declarations)
        fp (:fingerprint (catalog/check-operation-set! original))
        cases (:cases (outcome/derive-cases! source declarations))
        run #(argument/derive-catalog! source declarations fp cases)
        r (run)]
    (is (= :conditional-symbolic-catalog-derivation (:status r)) (pr-str r))
    (is (= (set (keys declarations)) (set (keys (:alternatives r)))))
    (is (= 4 (count (:outside-fragment r))))
    (is (= :not-established (:universal-refinement r)))
    (is (= :not-performed (:proof-discharge r)))
    (doseq [[id report] (:alternatives r)]
      (let [original (:construction (first (filter #(= id (:id %)) (:operations original))))]
        (is (= (:guard original) (:guard report)))
        (is (= (:replacements original) (:assignments report)))
        (is (= (:frame original) (:frame report)))
        (doseq [branches (filter vector? (vals (:equations report))) branch branches]
          (is (= (:expected branch) (:projected branch))))))
    (doseq [helper ["helper-1" "helper-2"]]
      (let [id (claim/alternative-id helper :accept)
            eq (get-in r [:alternatives id :equations])
            [present absent] (eq [:document :request_assignments "primary-r1"])]
        (is (= [:present :absent] (mapv :presence [present absent])))
        (is (= {:assignment/helper [:literal helper]} (:projected present) (:projected absent)))
        (is (= {:request/status [:literal :claimed] :request/revision [:literal 1]}
               (:projected (first (eq [:document :requests "r1"])))))))
    (is (= :not-established (:status (argument/derive-catalog! source declarations fp {}))))
    (with-redefs [writes/derive-transition! (constantly {})]
      (is (= :source-write-boundary-replaced (:reason (run)))))
    (let [branch @#'argument/document-branch]
      (with-redefs-fn {#'argument/document-branch
                       (fn [original entries present?]
                         (update (branch original entries present?) :projected
                                 assoc (first (keys entries)) [:literal :corrupted]))}
        #(is (= :source-write-equation-mismatch (:reason (run))))))))

(deftest symbolic-frame-creation-and-simultaneous-reads
  ;; Directly test the local algebra independently of the Claim fixture.
  (let [entries {:fixture/a [:nullable-document :rows "r" :a]
                 :fixture/b [:nullable-document :rows "r" :b]
                 :fixture/frame [:nullable-document :rows "r" :frame]}
        original {:state-types (zipmap (keys entries) (repeat {:type :integer :nullable? true}))
                  :input-types {} :writes #{:fixture/a :fixture/b}
                  :replacements {:fixture/a [:read :fixture/b] :fixture/b [:read :fixture/a]}}
        check #'argument/check-document!
        present (check original entries true) absent (check original entries false)]
    (is (= {:fixture/a [:pre :fixture/b] :fixture/b [:pre :fixture/a] :fixture/frame [:pre :fixture/frame]}
           (:projected present)))
    (is (= (zipmap (keys entries) (repeat [:literal nil])) (:projected absent)))
    (is (= #{:fixture/a :fixture/b} (:materialized-fields present)))
    (is (= (set (keys entries)) (:materialized-fields absent)))
    (let [original (assoc original :input-types {:fixture/input {:type :integer}}
                          :replacements {:fixture/a [:read :fixture/input] :fixture/b [:literal 42]})]
      (is (= {:fixture/a [:input :fixture/input] :fixture/b [:literal 42] :fixture/frame [:literal nil]}
             (:projected (check original entries false)))))))
