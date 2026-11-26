(ns gesso.assurance.reference-recovery-trajectory-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-recovery-trajectory :as trajectory]
            [gesso.assurance.reference-mapped-poststate-test :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]))

(def winner fixture/record)
(def loser-binding (assoc fixture/command-binding :command-id (identity/command-id "c2")
                           :principal (identity/principal "helper-b")))
(def candidate (resolution/command-record loser-binding {:status :claimed} "t1"))
(def refusal (resolution/command-record loser-binding {:status :refused} "t2"))
(def expected {:winner winner :abort candidate :recovery refusal})
(defn journal-put [record] [:put-docs :gesso_commands record])
(def winning (fixture/tx 1 [(journal-put winner)]))
(def aborted (fixture/tx 2 [(journal-put candidate) ["ASSERT FALSE"]]))
(def recovered (fixture/tx 3 [(journal-put refusal)]))
(def history [fixture/initial winning aborted recovered])
(def selected (subvec history 1))
(defn check! [h s e] (trajectory/compare-selected! h s e))

(deftest selected-linkage-is-conditional-and-historical
  (let [r (check! history selected expected)]
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched-retained-journal-trajectory (:recovery-linkage r)))
    (is (= [1 2 3] (:selected-tx-ids r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:provider-refinement r)))
    (is (= r (check! (conj history (fixture/tx 4 [(journal-put candidate)])) selected expected)))
    (is (= :nonadjacent-recovery-selection (:reason (check! history [winning recovered aborted] expected))))
    (is (= :unsupported (:status (check! (conj history winning) selected expected))))
    (is (= :recovery-command-linkage-mismatch (:reason (check! history selected (assoc expected :recovery candidate)))))))

(deftest invalid-recovery-linkage-fails-closed
  (let [other (resolution/command-record (assoc loser-binding :command-id (identity/command-id "c3")) {:status :refused} "t2")
        different (fixture/tx 3 [(journal-put other)])
        committed (fixture/tx 2 [(journal-put candidate)])
        altered (fixture/tx 3 [(journal-put refusal) (journal-put (assoc winner :gesso.command/outcome {:status :changed}))])
        preexisting (fixture/tx 0 [(journal-put winner)])]
    (doseq [[h s e reason] [[[fixture/initial winning aborted different] [winning aborted different] (assoc expected :recovery other) :recovery-command-linkage-mismatch]
                             [[fixture/initial winning committed recovered] [winning committed recovered] expected :recovery-commitment-sequence-mismatch]
                             [[fixture/initial winning aborted altered] [winning aborted altered] expected :recovery-command-linkage-mismatch]
                             [[preexisting winning aborted recovered] selected expected :recovery-journal-trajectory-mismatch]]]
      (let [r (check! h s e)]
        (is (= :mismatch (:status r)) (pr-str r))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :recovery-linkage)))))))
