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

(defn- worker-history []
  (let [id (java.util.UUID/fromString "10000000-0000-0000-0000-000000000001")
        lease (java.util.UUID/fromString "10000000-0000-0000-0000-000000000002")
        receipt (java.util.UUID/fromString "10000000-0000-0000-0000-000000000003")
        time #(-> (fixture/tx % []) :system-time)
        zone #(-> (time %) (.atZone (java.time.ZoneId/of "UTC")))
        pending {:xt/id id :gesso.live.outbox/type :gesso.live.outbox/publication-intent
                 :gesso.live.outbox/version 1 :gesso.live.outbox/source-id "s"
                 :gesso.live.outbox/status :pending :gesso.live.outbox/command-id (:xt/id winner)
                 :gesso.live.outbox/operation :example/claim :gesso.live.outbox/purpose :example/refresh
                 :extra "keep"}
        leased (assoc pending :gesso.live.outbox/status :leased
                      :gesso.live.outbox/lease-id lease :gesso.live.outbox/lease-owner "w"
                      :gesso.live.outbox/lease-until (time 60))
        acknowledged (assoc leased :gesso.live.outbox/status :dispatch-accepted
                            :gesso.live.outbox/dispatch-receipt-id receipt
                            :gesso.live.outbox/dispatch-adapter :example/adapter)
        w (update winning :ops conj [:put-docs :gesso_live_outbox pending])
        acquire (fixture/tx 2 [["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)" id (time 1)]
                               ["ASSERT CURRENT_TIMESTAMP < ?" (time 60)]
                               ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" (time 60)]
                               [:put-docs :gesso_live_outbox leased]])
        a (fixture/tx 3 (:ops aborted))
        ack (fixture/tx 4 [["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)" id (zone 2) (zone 60)]
                           [:put-docs :gesso_live_outbox (assoc acknowledged :gesso.live.outbox/lease-until (zone 60))]])
        r (fixture/tx 5 (:ops recovered))]
    {:history [fixture/initial w acquire a ack r] :selected [w a r] :id id}))

(deftest exact-worker-interleavings-preserve-the-recovery-linkage
  (let [{:keys [history selected id]} (worker-history)
        result (trajectory/compare-worker-interleaved! history selected expected "s")]
    (is (= :matched (:status result)) (pr-str result))
    (is (= trajectory/interleaved-profile (:profile result)))
    (is (= :matched-retained-journal-trajectory (:recovery-linkage result)))
    (is (= [1 3 5] (:selected-tx-ids result)))
    (is (= [{:tx-id 2 :maintenance :lease-acquisition :intent-id id}
            {:tx-id 4 :maintenance :dispatch-acknowledgment :intent-id id}]
           (:intervening-maintenance result)))
    (is (= 6 (:replayed-transaction-count result)))
    (is (= :none (:authority result)))
    (is (= :not-issued (:certificate result)))
    (is (= :not-established (:provider-refinement result)))
    (is (= :nonadjacent-recovery-selection (:reason (check! history selected expected))))
    (is (= 7 (:replayed-transaction-count (trajectory/compare-worker-interleaved!
                                          (conj history (fixture/tx 6 [["ASSERT TRUE"]])) selected expected "s"))))
    (is (= :unsupported-recovery-linkage-replay (:reason (trajectory/compare-worker-interleaved!
                                                          (conj history (fixture/tx 6 [[:unsupported]])) selected expected "s"))))
    (is (= :invalid-interleaved-recovery-selection (:reason (trajectory/compare-worker-interleaved!
                                                            history (vec (reverse selected)) expected "s"))))
    (is (= :unqualified-intervening-worker (:reason (trajectory/compare-worker-interleaved! history selected expected "wrong"))))))

(deftest worker-interleaving-refuses-non-stuttering-or-inexact-steps
  (let [{:keys [history selected]} (worker-history)
        run! #(trajectory/compare-worker-interleaved! % selected expected "s")
        time #(-> (fixture/tx % []) :system-time)
        changes [(update-in history [2 :ops] #(into [["ASSERT TRUE"]] %))
                 (assoc-in history [2 :ops 3 2 :extra] "changed")
                 (assoc-in history [2 :ops 0 2] (time 0))
                 (assoc-in history [2 :ops] [["ASSERT TRUE"]])
                 (update-in history [2 :ops] #(into [[:put-docs :gesso_requests fixture/row]] %))
                 (update-in history [4 :ops] #(into [["ASSERT FALSE"]] %))
                 (update-in history [0 :ops] conj [:put-docs {:into :gesso_requests :valid-from (time 2)}
                                                  (assoc fixture/row :xt/id "future")])
                 (update-in history [0 :ops] conj [:put-docs {:into :gesso_requests :valid-from (.plusMillis (time 1) 100)
                                                             :valid-to (.plusMillis (time 1) 200)}
                                                  (assoc fixture/row :xt/id "temporary")])]]
    (doseq [h changes]
      (let [r (run! h)]
        (is (= :mismatch (:status r)) (pr-str r))
        (is (= :unqualified-intervening-worker (:reason r)))
        (is (not-any? #(contains? r %) [:recovery-linkage :selected-tx-ids :intervening-maintenance]))))))
