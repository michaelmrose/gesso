(ns gesso.live.outbox.reconciliation-test
  "Physical single-node observation of interrupted cycle lease/receipt debt."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.reconciliation :as reconciliation]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant ZoneOffset]))

(def source-id "gesso/test-reconciliation")

(defn- seed! [node id]
  (let [p (outbox/prepare!
           {:intent-ids [id] :source-id source-id
            :operation :request/claim :purpose :gesso.live/refresh}
           [{:topic :request :id "r-1" :change/kind :updated}])]
    (xt/execute-tx node (conj (:guard-ops p) (:tx-op p)))))

(defn- claim! [node id worker]
  (let [now (Instant/now)
        token (random-uuid)
        r (acquisition/acquire-pending!
           node {:intent-id id :lease-id token :worker-id worker
                 :now now :lease-until (.plusSeconds now 120)
                 :source-id source-id})]
    (is (= :acquired (:status r)))
    {:intent-id id :lease-id token :worker-id worker}))

(defn- envelope [claim]
  {:error/type :gesso.live.outbox/cycle-interrupted
   :phase :delivery :rescan-required? true
   :in-flight claim})

(defn- physical-row [node id]
  (first (xt/q node
               ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?"
                id])))

(defn- rejection [node source failure]
  (try
    (reconciliation/inspect! node source failure)
    nil
    (catch clojure.lang.ExceptionInfo e
      (when (= :gesso.live.outbox/invalid-reconciliation
               (:error/type (ex-data e)))
        (:reason (ex-data e))))))

(deftest interruption-reconciliation-requires-a-credible-host-envelope-test
  (let [id (random-uuid)
        claim {:intent-id id :lease-id (random-uuid) :worker-id "worker-a"}
        valid (envelope claim)]
    ;; Validation must finish BEFORE touching the provider.
    (doseq [[candidate reason]
            [[nil :invalid-interruption-envelope]
             [(assoc valid :rescan-required? false)
              :invalid-interruption-envelope]
             [(assoc valid :error/type :other/failure)
              :invalid-interruption-envelope]
             [(assoc-in valid [:in-flight :intent-id] "not-uuid")
              :invalid-in-flight]
             [(assoc-in valid [:in-flight :worker-id] " ")
              :invalid-in-flight]
             [(assoc-in valid [:in-flight :receipt-id] (random-uuid))
              :invalid-reported-receipt]
             [(assoc-in valid [:in-flight :receipt-evidence]
                        :unverified-post-dispatch-receipt)
              :invalid-reported-receipt]
             [(assoc-in valid [:in-flight :receipt-id] "forged")
              :invalid-reported-receipt]]]
      (is (= reason (rejection :no-node source-id candidate))))
    (is (= :invalid-source-id (rejection :no-node " " valid)))
    (is (= {:status :unidentified :rescan-required? true}
           (reconciliation/inspect! :no-node source-id
                                    (envelope nil))))))

(deftest physical-recovery-observes-pending-lease-and-terminal-receipt-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          unknown (envelope {:intent-id id
                             :lease-id (random-uuid)
                             :worker-id "worker-a"})]
      (is (= :missing (:status (reconciliation/inspect! node source-id unknown))))
      (seed! node id)
      (is (= :pending-after-interruption
             (:status (reconciliation/inspect! node source-id unknown))))
      (let [lease (claim! node id "worker-a")
            base (envelope lease)
            receipt-id (random-uuid)
            reported (envelope (assoc lease
                                      :receipt-id receipt-id
                                      :receipt-evidence
                                      :unverified-post-dispatch-receipt))]
        (is (= :same-lease-active
               (:status (reconciliation/inspect! node source-id reported))))
        (let [ack (acknowledgment/acknowledge-dispatch!
                   node {:intent-id id :lease-id (:lease-id lease)
                         :worker-id "worker-a"
                         :receipt {:receipt-id receipt-id
                                   :adapter :gesso.live/synchronous-source
                                   :result :accepted}})]
          (is (= :acknowledged (:status ack))))
        (let [result (reconciliation/inspect! node source-id reported)]
          (is (= :matching-acknowledgment-recorded (:status result)))
          (is (= :dispatch-accepted (:observed-status result)))
          (is (some? (:observed-system-from result)))
          (is (= true (:rescan-required? result)))
          (is (not (contains? result :may-replay?))))
        (is (= :terminal-without-receipt-correlation
               (:status (reconciliation/inspect! node source-id base))))
        (is (= :other-receipt-recorded
               (:status (reconciliation/inspect!
                         node source-id
                         (assoc-in reported [:in-flight :receipt-id]
                                   (random-uuid))))))
        (is (= :other-lease-recorded
               (:status (reconciliation/inspect!
                         node source-id
                         (envelope (assoc lease :lease-id (random-uuid)))))))
        (is (= :source-mismatch
               (rejection node "foreign/source" reported)))))))

(deftest physical-recovery-never-mistakes-reclaim-for-original-ack-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)]
      (seed! node id)
      (let [old (claim! node id "old-worker")
            report (envelope (assoc old
                                    :receipt-id (random-uuid)
                                    :receipt-evidence
                                    :unverified-post-dispatch-receipt))
            original (physical-row node id)
            replacement (-> original
                            (dissoc :xt/system-from)
                            (assoc :gesso.live.outbox/lease-id (random-uuid)
                                   :gesso.live.outbox/lease-owner "new-worker"))]
        ;; Physical change stands in for a reclaimed lease; the inspected
        ;; lease ID cannot grant authority over the new physical owner.
        (xt/execute-tx node [[:put-docs outbox/intent-table replacement]])
        (is (= :lease-replaced
               (:status (reconciliation/inspect! node source-id report))))
        (is (= :leased (:gesso.live.outbox/status (physical-row node id))))))))

(deftest malformed-physical-reconciliation-data-fails-closed-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          claim {:intent-id id :lease-id (random-uuid)
                 :worker-id "worker-a"}
          report (envelope claim)]
      (seed! node id)
      (let [row (physical-row node id)]
        (xt/execute-tx node [[:put-docs outbox/intent-table
                              (assoc (dissoc row :xt/system-from)
                                     :gesso.live.outbox/status :dispatch-accepted
                                     :gesso.live.outbox/lease-id (:lease-id claim)
                                     :gesso.live.outbox/lease-owner "worker-a"
                                     :gesso.live.outbox/lease-until
                                     (.atZone (Instant/now) ZoneOffset/UTC))]])
        (is (= :invalid-physical-receipt
               (rejection node source-id report)))))))

(deftest real-cycle-interruption-reconciles-physically-recorded-receipt-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          receipt-id (random-uuid)
          _ (seed! node id)
          failure
          (with-redefs
            [delivery/dispatch-acquired!
             (fn [_ _ _ acquired _]
               ;; The trusted delivery adapter submits its guarded XTDB
               ;; acknowledgment, but its caller loses the successful reply.
               ;; The real cycle must retain the unverified receipt and
               ;; stop instead of certifying source/browser delivery.
               (let [ack (acknowledgment/acknowledge-dispatch!
                          node {:intent-id (:intent-id acquired)
                                :lease-id (:lease-id acquired)
                                :worker-id (:worker-id acquired)
                                :receipt {:receipt-id receipt-id
                                          :adapter :gesso.live/synchronous-source
                                          :result :accepted}})]
                 (is (= :acknowledged (:status ack))))
               (throw
                (ex-info "reply lost after physical XTDB acknowledgment"
                         {:error/type :gesso.live.outbox/post-dispatch-ack-unknown
                          :intent-id (:intent-id acquired)
                          :lease-id (:lease-id acquired)
                          :receipt-id receipt-id
                          :dispatch/status :accepted
                          :acknowledgment/status :unknown})))]
            (try
              (cycle/process-cycle! node {:source :installed} {} source-id
                                    {:worker-id "worker-a" :max-items 1})
              (is false "the adapter's lost reply cannot complete the cycle")
              nil
              (catch clojure.lang.ExceptionInfo error
                (ex-data error))))]
      (is (= :gesso.live.outbox/cycle-interrupted (:error/type failure)))
      (is (= :delivery (:phase failure)))
      (is (= :unverified-post-dispatch-receipt
             (get-in failure [:in-flight :receipt-evidence])))
      (is (= receipt-id (get-in failure [:in-flight :receipt-id])))
      (is (= :matching-acknowledgment-recorded
             (:status (reconciliation/inspect! node source-id failure))))
      (is (= :dispatch-accepted
             (:gesso.live.outbox/status (physical-row node id)))))))
