(ns gesso.live.outbox.reconciliation-test
  "Physical single-node observation of interrupted cycle lease/receipt debt."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.outbox :as outbox]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.reconciliation :as reconciliation]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
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

(defn- committed-prefix [intent-id lease-id receipt-id]
  {:intent-id intent-id
   :worker-id "worker-a"
   :acquisition-status :acquired
   :status :dispatch-accepted
   :lease-id lease-id
   :receipt-id receipt-id
   :progression {:basis :reported}})

(defn- completed-envelope [completed claim]
  (assoc (envelope claim)
         :completed completed
         :completed-count (count completed)
         :last-completed-cursor (some-> completed peek :intent-id)))

(deftest completed-prefix-validation-rejects-contradictions-before-physical-reads-test
  (let [a (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")
        b (java.util.UUID/fromString "00000000-0000-0000-0000-000000000002")
        x (committed-prefix a (random-uuid) (random-uuid))
        y (committed-prefix b (random-uuid) (random-uuid))
        baseline (completed-envelope [x y] nil)]
    (doseq [invalid [(assoc baseline :completed-count 1)
                     (assoc baseline :completed [y x])
                     (assoc baseline :completed [x x])
                     (assoc baseline :last-completed-cursor a)
                     (assoc baseline :completed (vec (repeat 129 x)))
                     (assoc baseline :completed [42])
                     (assoc baseline :completed [(dissoc x :receipt-id) y])
                     (assoc baseline :completed [(dissoc x :worker-id) y])
                     (assoc baseline :completed [(assoc x :worker-id " ") y])
                     (assoc baseline :completed [(assoc x :lease-id "bad") y])
                     (assoc baseline :completed [(assoc x :status :unsent) y])
                     (assoc baseline :completed [(assoc x :acquisition-status
                                                        :contended) y])
                     (assoc baseline :completed [(dissoc x :progression) y])
                     (assoc baseline :completed :not-a-vector)]]
      ;; :no-node would fail as a provider if validation performed a read.
      (is (= :invalid-completed-prefix
             (rejection :no-node source-id invalid))))))

(deftest recorded-prefix-requires-matching-physical-receipts-test
  (with-open [node (xtn/start-node {})]
    (let [a (java.util.UUID/fromString "00000000-0000-0000-0000-000000000011")
          b (java.util.UUID/fromString "00000000-0000-0000-0000-000000000012")
          c (java.util.UUID/fromString "00000000-0000-0000-0000-000000000013")
          _ (doseq [id [a b c]] (seed! node id))
          ca (claim! node a "worker-a")
          cb (claim! node b "worker-a")
          r-a (random-uuid)
          r-b (random-uuid)]
      (doseq [[lease receipt] [[ca r-a] [cb r-b]]]
        (is (= :acknowledged
               (:status (acknowledgment/acknowledge-dispatch!
                         node {:intent-id (:intent-id lease)
                               :lease-id (:lease-id lease)
                               :worker-id (:worker-id lease)
                               :receipt {:receipt-id receipt
                                         :adapter :gesso.live/synchronous-source
                                         :result :accepted}})))))
      (let [completed [(committed-prefix a (:lease-id ca) r-a)
                       (committed-prefix b (:lease-id cb) r-b)]
            report (completed-envelope completed nil)
            result (reconciliation/inspect! node source-id report)]
        (is (= :unidentified (:status result)))
        (is (true? (:rescan-required? result)))
        (is (= [a b] (mapv :intent-id (:completed-observations result))))
        (is (= [:matching-acknowledgment-recorded
                :matching-acknowledgment-recorded]
               (mapv :status (:completed-observations result))))
        (is (every? :observed-system-from (:completed-observations result)))
        (is (not (contains? result :may-replay?)))
        ;; Changing ONLY the physical worker identity while retaining the
        ;; old lease token and receipt must invalidate reconciliation.
        ;; This cannot be inferred from lease/receipt equality alone.
        (let [row (physical-row node b)
              wrong-owner (-> row
                              (dissoc :xt/system-from)
                              (assoc :gesso.live.outbox/lease-owner "other-worker"))]
          (xt/execute-tx node [[:put-docs outbox/intent-table wrong-owner]])
          (is (= [:matching-acknowledgment-recorded
                  :different-acknowledgment-recorded]
                 (mapv :status
                       (:completed-observations
                        (reconciliation/inspect! node source-id report))))
              "A matching token and receipt with a foreign owner is not our completion")
          ;; Restore the original physical evidence to prove this check is
          ;; the owner identity, not an unrelated terminal-status failure.
          (xt/execute-tx node [[:put-docs outbox/intent-table
                                (dissoc row :xt/system-from)]])
          (is (= [:matching-acknowledgment-recorded
                  :matching-acknowledgment-recorded]
                 (mapv :status
                       (:completed-observations
                        (reconciliation/inspect! node source-id report))))
              "Restoring the correct owner restores qualified observation")
          ;; Also preserve the existing receipt-change negative test.
          (let [modified (assoc (dissoc row :xt/system-from)
                                :gesso.live.outbox/dispatch-receipt-id
                                (random-uuid))]
            (xt/execute-tx node [[:put-docs outbox/intent-table modified]]))
          (is (= [:matching-acknowledgment-recorded
                  :different-acknowledgment-recorded]
                 (mapv :status
                       (:completed-observations
                        (reconciliation/inspect! node source-id report))))))
          (is (= :source-mismatch
                 (rejection node "foreign/source" report)))
        ;; A third row may exist without being part of the completed prefix;
        ;; it must not be invented into the report.
        (is (= 2 (count (:completed-observations result))))))))

(deftest completed-prefix-reports-lost-and-nonterminal-physical-rows-test
  (with-open [node (xtn/start-node {})]
    (let [a (java.util.UUID/fromString "00000000-0000-0000-0000-000000000021")
          b (java.util.UUID/fromString "00000000-0000-0000-0000-000000000022")
          c (java.util.UUID/fromString "00000000-0000-0000-0000-000000000023")
          _ (seed! node b)
          _ (seed! node c)
          lease-c (claim! node c "worker-c")
          report (completed-envelope
                  [(committed-prefix a (random-uuid) (random-uuid))
                   (committed-prefix b (random-uuid) (random-uuid))
                   (committed-prefix c (:lease-id lease-c) (random-uuid))]
                  nil)
          found (:completed-observations
                 (reconciliation/inspect! node source-id report))]
      (is (= [:missing :not-terminal :not-terminal]
             (mapv :status found)))
      (is (= [:pending :leased] (mapv :observed-status (rest found)))))))

(deftest a-contended-prefix-is-not-a-dispatch-and-needs-no-provider-test
  (let [a (java.util.UUID/fromString "00000000-0000-0000-0000-000000000031")
        b (java.util.UUID/fromString "00000000-0000-0000-0000-000000000032")
        completed [{:intent-id a :acquisition-status :contended}
                   {:intent-id b :acquisition-status :unavailable
                    :reason :not-leased}]
        result (reconciliation/inspect! :no-node source-id
                                        (completed-envelope completed nil))]
    (is (= :unidentified (:status result)))
    (is (= [{:intent-id a :status :no-dispatch-claim}
            {:intent-id b :status :no-dispatch-claim}]
           (:completed-observations result)))))

(deftest real-cycle-failure-inspects-completed-prefix-and-in-flight-test
  (with-open [node (xtn/start-node {})]
    (let [a (java.util.UUID/fromString "00000000-0000-0000-0000-000000000041")
          b (java.util.UUID/fromString "00000000-0000-0000-0000-000000000042")
          _ (doseq [id [a b]] (seed! node id))
          calls (atom 0)
          failure
          (with-redefs
            [delivery/dispatch-acquired!
             (fn [_ _ _ acquired _]
               (swap! calls inc)
               (let [receipt-id (random-uuid)
                     ack (acknowledgment/acknowledge-dispatch!
                          node {:intent-id (:intent-id acquired)
                                :lease-id (:lease-id acquired)
                                :worker-id (:worker-id acquired)
                                :receipt {:receipt-id receipt-id
                                          :adapter :gesso.live/synchronous-source
                                          :result :accepted}})]
                 (is (= :acknowledged (:status ack)))
                 (if (= 1 @calls)
                   {:intent-id (:intent-id acquired)
                    :lease-id (:lease-id acquired)
                    :receipt-id receipt-id
                    :status :dispatch-accepted
                    :progression (:progression acquired)
                    :acknowledgment {:status :acknowledged
                                     :intent-id (:intent-id acquired)
                                     :lease-id (:lease-id acquired)
                                     :receipt-id receipt-id}
                    :source-result {:status :emitted-many :count 1
                                    :results [{:status :emitted}]}}
                   (throw
                    (ex-info "second acknowledgment was recorded but reply lost"
                             {:error/type :gesso.live.outbox/post-dispatch-ack-unknown
                              :intent-id (:intent-id acquired)
                              :lease-id (:lease-id acquired)
                              :receipt-id receipt-id
                              :dispatch/status :accepted
                              :acknowledgment/status :unknown})))))]
            (try
              (cycle/process-cycle! node {:source :installed} {} source-id
                                    {:worker-id "worker-a" :max-items 2})
              (is false "second lost reply must stop the cycle")
              nil
              (catch clojure.lang.ExceptionInfo e (ex-data e))))
          examined (reconciliation/inspect! node source-id failure)]
      (is (= 2 @calls))
      (is (= :gesso.live.outbox/cycle-interrupted (:error/type failure)))
      (is (= 1 (:completed-count failure)))
      (is (= a (get-in failure [:completed 0 :intent-id])))
      (is (= "worker-a" (get-in failure [:completed 0 :worker-id]))
          "Real completed prefix must preserve its actual lease owner")
      (is (= b (get-in failure [:in-flight :intent-id])))
      (is (= :matching-acknowledgment-recorded (:status examined)))
      (is (= a (get-in examined [:completed-observations 0 :intent-id])))
      (is (= :matching-acknowledgment-recorded
             (get-in examined [:completed-observations 0 :status])))
      (is (true? (:rescan-required? examined)))
      (is (not (contains? examined :may-replay?))))))

(deftest reconciliation-awaits-the-selected-provider-frontier-test
  (let [id (random-uuid)
        report (envelope {:intent-id id :lease-id (random-uuid)
                          :worker-id "worker-a"})
        node :selected-node
        calls (atom [])
        opts {:await-token (basis/->tx-basis-str {"xtdb" [42]})}]
    (with-redefs [xt/status (fn [n]
                             (swap! calls conj [:status n])
                             {:latest-submitted-msg-ids {"xtdb" [42]}})
                  live.xtdb/q-from (fn [n query actual]
                                     (swap! calls conj [:query n query actual])
                                     [])]
      (is (= :missing (:status (reconciliation/inspect! node source-id report))))
      (is (= [[:status node]
              [:query node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id] opts]]
             @calls))
      (reset! calls [])
      (with-redefs [xt/status (constantly {:latest-submitted-msg-ids {}})]
        (is (= ::frontier/invalid-frontier
               (try (reconciliation/inspect! node source-id report)
                    nil (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e))))))
        (is (empty? @calls)))
      (let [lost (InterruptedException. "interrupted reconciliation")]
        (try
          (with-redefs [live.xtdb/q-from (fn [& _] (throw lost))]
            (is (= [lost true]
                   (try (reconciliation/inspect! node source-id report)
                        nil (catch Exception e [e (.isInterrupted (Thread/currentThread))])))))
          (finally (Thread/interrupted)))))))

(deftest reconciliation-observes-submitted-acknowledgment-without-client-await-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          lease (claim! node id "worker-a")
          receipt-id (random-uuid)
          report (envelope (assoc lease :receipt-id receipt-id
                                  :receipt-evidence :unverified-post-dispatch-receipt))
          row (physical-row node id)
          terminal (-> row
                       (dissoc :xt/system-from)
                       (assoc :gesso.live.outbox/status :dispatch-accepted
                              :gesso.live.outbox/dispatch-receipt-id receipt-id
                              :gesso.live.outbox/dispatch-adapter :gesso.live/synchronous-source))]
      ;; Simulate a submitted terminal write whose response was lost. Do not
      ;; execute-tx or issue another query between submission and inspection.
      (xt/submit-tx node [[:put-docs outbox/intent-table terminal]])
      (let [observed (reconciliation/inspect! node source-id report)]
        (is (= :matching-acknowledgment-recorded (:status observed)))
        (is (true? (:rescan-required? observed)))
        (is (not (contains? observed :may-replay?)))))))
