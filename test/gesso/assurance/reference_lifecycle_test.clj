(ns gesso.assurance.reference-lifecycle-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-lifecycle :as lifecycle]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-reconciliation :as reference-reconciliation]
            [gesso.live.outbox.reconciliation :as reconciliation]
            [gesso.assurance.reference-temporal-fixtures :refer [sys provider-rectangles coalesce-valid]]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import (java.time Instant)))

(defn- id [n] (java.util.UUID. 0 n))
(defn- time-at [n] (.plusSeconds ^Instant (sys 1) n))
(defn- publish [n ids value]
  {:kind :publish :system-time (time-at n)
   :spec {:intent-ids ids :source-id "lifecycle" :operation :request/claim
          :purpose :gesso.live/refresh :command-id "command-1"}
   :changes (mapv (fn [i] {:topic :request :id (str i) :change/kind :updated}) ids)
   :writes [[:put-docs :lifecycleprobe {:xt/id "domain" :value value}]]})
(defn- acquire [kind n observed intent lease owner deadline]
  {:kind kind :system-time (time-at n) :observed-system (time-at observed)
   :observed-valid (time-at observed)
   :claim {:intent-id (id intent) :lease-id (id lease) :worker-id owner
           :source-id "lifecycle" :now (time-at n) :lease-until (time-at deadline)}})
(defn- ack [n observed intent lease owner receipt]
  {:kind :acknowledge :system-time (time-at n) :observed-system (time-at observed)
   :observed-valid (time-at observed)
   :submission {:intent-id (id intent) :lease-id (id lease) :worker-id owner
                :receipt {:receipt-id (id receipt) :adapter :test/adapter :result :accepted}}})

(def ^:private steps
  [(publish 0 [(id 1) (id 2)] 1)
   (publish 1 [(id 3) (id 2)] 2)
   (acquire :acquire 2 1 1 11 "a" 10)
   (acquire :acquire 3 1 1 12 "b" 20)
   (acquire :reclaim 4 2 1 12 "b" 20)
   (acquire :reclaim 10 4 1 12 "b" 20)
   (ack 11 2 1 11 "a" 21)
   (ack 12 10 1 12 "b" 22)
   (ack 13 12 1 12 "b" 22)
   (acquire :acquire 14 12 2 13 "c" 30)
   (ack 15 14 2 13 "c" 23)])
(def ^:private outcomes [:published :contended :acquired :contended :contended
                         :reclaimed :contended :acknowledged :unavailable :acquired :acknowledged])

(deftest closed-reference-publication-lifecycle
  (let [r (lifecycle/predict-lifecycle steps)
        visible #(temporal/visible-at (:replay r) (time-at 15) (time-at %))
        intents (get (visible 15) :gesso_live_outbox)]
    (is (= :predicted (:status r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= outcomes (mapv :outcome (:trace r))))
    (is (= 10 (count (:transactions r))))
    (is (= [true false true false false true false true true true]
           (mapv :committed? (get-in r [:replay :history]))))
    (is (= #{(id 1) (id 2)} (set (keys intents))))
    (is (= [:dispatch-accepted :dispatch-accepted] (mapv #(get-in intents [(id %) :gesso.live.outbox/status]) [1 2])))
    (is (= [(id 22) (id 23)] (mapv #(get-in intents [(id %) :gesso.live.outbox/dispatch-receipt-id]) [1 2])))
    (is (= (id 11) (get-in (visible 9) [:gesso_live_outbox (id 1) :gesso.live.outbox/lease-id])))
    (is (= (id 12) (get-in (visible 10) [:gesso_live_outbox (id 1) :gesso.live.outbox/lease-id])))
    (is (= 1 (get-in (visible 15) [:lifecycleprobe "domain" :value])))
    (is (= [] (get-in r [:trace 8 :ops])))
    (doseq [invalid [[] (subvec steps 1) (assoc-in steps [0 :spec :intent-ids] [(id 1) (id 1)])
                     (assoc-in steps [0 :writes] [[:put-docs :gesso_live_outbox {:xt/id (id 3)}]])
                     (assoc-in steps [0 :changes 0 :extra] true)
                     (assoc-in steps [1 :system-time] (time-at 0))
                     (assoc-in steps [10 :submission :receipt :result] :failed)]]
      (let [p (lifecycle/predict-lifecycle invalid)]
        (is (= :unsupported (:status p)))
        (is (not-any? #(contains? p %) [:trace :transactions :replay]))))))

(defn- execute-publication! [node {:keys [spec changes writes system-time]} captured]
  (let [{:keys [guard-ops tx-op]} (outbox/prepare! spec changes)
        ops (into (into guard-ops writes) [tx-op])]
    (swap! captured conj ops)
    (try (live.xtdb/execute-tx! node ops {:system-time system-time}) {:status :published}
         (catch Throwable e
           (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
             {:status :contended} (throw e))))))

(deftest independent-lifecycle-matches-production-and-xtdb
  (let [prediction (lifecycle/predict-lifecycle steps)]
    (is (= :predicted (:status prediction)))
    (with-open [node (xtn/start-node {})]
      (let [execute! live.xtdb/execute-tx!]
        (doseq [[{:keys [kind system-time observed-system observed-valid claim submission] :as step} predicted]
                (map vector steps (:trace prediction))]
          (let [captured (atom [])
                actual (if (= :publish kind)
                         (execute-publication! node step captured)
                         (with-redefs [frontier/q-from! (fn [_ [_ id]]
                                                          (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?"
                                                                      observed-valid observed-system id]))
                                       live.xtdb/execute-tx! (fn [connectable ops]
                                                               (swap! captured conj ops)
                                                               (execute! connectable ops {:system-time system-time}))]
                           (case kind
                             :acquire (acquisition/acquire-pending! node claim)
                             :reclaim (acquisition/reclaim-expired! node claim)
                             :acknowledge (acknowledgment/acknowledge-dispatch! node submission))))]
            (is (= (:outcome predicted) (:status actual)))
            (is (= (:reason predicted) (:reason actual)))
            (is (= (if (seq (:ops predicted)) [(:ops predicted)] []) @captured)))))
      (is (= (frequencies (coalesce-valid (get-in prediction [:replay :rectangles])))
             (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) [:gesso_live_outbox :lifecycleprobe])))))
      (doseq [s (range 0 16) v (range 0 16) table [:gesso_live_outbox :lifecycleprobe]]
        (let [rows (xt/q node [(str "SELECT * FROM " (name table) " FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ?")
                               (time-at v) (time-at s)])]
          (is (= (count rows) (count (set (map :xt/id rows)))))
          (is (= (get (temporal/visible-at (:replay prediction) (time-at s) (time-at v)) table {})
                 (into {} (map (juxt :xt/id identity)) rows))))))))

(defn- interruption [intent lease owner receipt]
  {:error/type :gesso.live.outbox/cycle-interrupted :rescan-required? true
   :in-flight (cond-> {:intent-id (id intent) :lease-id (id lease) :worker-id owner}
                receipt (assoc :receipt-id (id receipt)
                               :receipt-evidence :unverified-post-dispatch-receipt))})

(defn- pinned-read [node s v]
  (fn [_ [_ id]]
    (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?" v s id])))

(defn- submit-history! [node transactions]
  (doseq [{:keys [system-time ops]} transactions]
    (try (live.xtdb/execute-tx! node ops {:system-time system-time})
         (catch Throwable e
           (when-not (= :xtdb/assert-failed (:xtdb.error/code (ex-data e))) (throw e))))))

(deftest independent-in-flight-reconciliation
  (let [r (lifecycle/predict-lifecycle steps)
        prior (:replay r)
        cases [[0 (interruption 1 11 "a" nil) :pending-after-interruption]
               [2 (interruption 1 11 "a" nil) :same-lease-active]
               [9 (interruption 1 11 "a" 21) :same-lease-active]
               [10 (interruption 1 11 "a" 21) :lease-replaced]
               [12 (interruption 1 11 "a" 21) :other-lease-recorded]
               [12 (interruption 1 12 "b" nil) :terminal-without-receipt-correlation]
               [12 (interruption 1 12 "b" 22) :matching-acknowledgment-recorded]
               [12 (interruption 1 12 "b" 21) :other-receipt-recorded]
               [12 (interruption 1 12 "a" 22) :other-lease-recorded]
               [15 (interruption 3 11 "a" nil) :missing]
               [15 (dissoc (interruption 1 11 "a" nil) :in-flight) :unidentified]]]
    (with-open [node (xtn/start-node {})]
      (submit-history! node (:transactions r))
      (doseq [[n failure expected] cases]
        (let [s (time-at n)
              p (reference-reconciliation/predict-inspection prior s s "lifecycle" failure)
              actual (with-redefs [frontier/q-from! (pinned-read node s s)]
                       (reconciliation/inspect! node "lifecycle" failure))]
          (is (= :predicted (:status p)))
          (is (= expected (:status actual)))
          (is (= (:observation p) actual))
          (is (true? (:rescan-required? actual)))
          (is (= :none (:authority p)))
          (is (= :not-issued (:certificate p)))
          (is (not (contains? actual :may-replay?)))))
      (doseq [[source failure] [["foreign" (interruption 1 12 "b" 22)]
                               ["lifecycle" (assoc (interruption 1 12 "b" 22) :completed [])]
                               ["lifecycle" (assoc (interruption 1 12 "b" 22) :rescan-required? false)]
                               ["lifecycle" (update (interruption 1 12 "b" 22) :in-flight dissoc :receipt-evidence)]]]
        (let [p (reference-reconciliation/predict-inspection prior (time-at 15) (time-at 15) source failure)]
          (is (= :unsupported (:status p)))
          (is (not (contains? p :observation))))))))

(deftest lost-acknowledgment-reply-does-not-establish-noncommit
  ;; Three physical hypotheses with indistinguishable caller exceptions:
  ;; no submission, committed submission, and expired assertion abort.
  (doseq [[mode n expected] [[:not-submitted 3 :same-lease-active]
                            [:committed 3 :matching-acknowledgment-recorded]
                            [:aborted 10 :same-lease-active]]]
    (let [prefix (subvec steps 0 3)
          attempt (ack n 2 1 11 "a" 21)
          history (lifecycle/predict-lifecycle (cond-> prefix (not= :not-submitted mode) (conj attempt)))
          s (time-at (if (= :not-submitted mode) 2 n))
          failure (interruption 1 11 "a" 21)
          predicted (reference-reconciliation/predict-inspection (:replay history) s s "lifecycle" failure)
          lost (ex-info "Lost acknowledgment response" {:test/outcome :unknown})]
      (is (= :predicted (:status history)))
      (is (= expected (get-in predicted [:observation :status])))
      (with-open [node (xtn/start-node {})]
        (submit-history! node (:transactions (lifecycle/predict-lifecycle prefix)))
        (let [execute! live.xtdb/execute-tx!
              observed (time-at 2)
              actual-error
              (with-redefs [frontier/q-from! (pinned-read node observed observed)
                            live.xtdb/execute-tx! (fn [connectable ops]
                                                    (when-not (= :not-submitted mode)
                                                      (let [outcome (try (execute! connectable ops {:system-time (time-at n)}) :committed
                                                                         (catch Throwable e
                                                                           (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e))) :aborted (throw e))))]
                                                        (is (= mode outcome))))
                                                    (throw lost))]
                (try (acknowledgment/acknowledge-dispatch! node (:submission attempt))
                     (catch Throwable e e)))]
          (is (identical? lost actual-error)))
        (let [actual (with-redefs [frontier/q-from! (pinned-read node s s)]
                       (reconciliation/inspect! node "lifecycle" failure))]
          (is (= (:observation predicted) actual))
          (is (true? (:rescan-required? actual)))
          (is (not (contains? actual :may-replay?))))
        (is (= (frequencies (coalesce-valid (get-in history [:replay :rectangles])))
               (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) [:gesso_live_outbox :lifecycleprobe])))))))))

(defn- completed-claim [intent lease owner receipt acquisition-status]
  {:intent-id (id intent) :lease-id (id lease) :worker-id owner
   :receipt-id (id receipt) :acquisition-status acquisition-status
   :status :dispatch-accepted :progression {:basis :reported}})

(defn- with-completed [failure entries]
  (assoc failure :completed entries :completed-count (count entries)
         :last-completed-cursor (:intent-id (peek entries))))

(deftest independently-predicted-completed-prefix-reconciliation
  (let [r (lifecycle/predict-lifecycle steps)
        prior (:replay r)
        a (completed-claim 1 12 "b" 22 :reclaimed)
        b (completed-claim 2 13 "c" 23 :acquired)
        missing (completed-claim 3 14 "d" 24 :acquired)
        skipped {:intent-id (id 4) :acquisition-status :contended :reason :stale}
        unavailable {:intent-id (id 5) :acquisition-status :unavailable :reason :missing}
        failure (interruption 2 13 "c" 23)
        no-flight (dissoc failure :in-flight)
        cases [[(with-completed failure [a skipped unavailable]) [12 14]
                [:matching-acknowledgment-recorded :no-dispatch-claim :no-dispatch-claim] :same-lease-active]
               [(with-completed failure [a b missing]) [10 14 15 15]
                [:not-terminal :not-terminal :missing] :matching-acknowledgment-recorded]
               [(with-completed no-flight [a b]) [12 15]
                [:matching-acknowledgment-recorded :matching-acknowledgment-recorded] :unidentified]
               [(with-completed no-flight [(assoc a :worker-id "wrong") (assoc b :receipt-id (id 21))]) [15 15]
                [:different-acknowledgment-recorded :different-acknowledgment-recorded] :unidentified]
               [(with-completed no-flight [(assoc a :lease-id (id 11))]) [15]
                [:different-acknowledgment-recorded] :unidentified]
               [(with-completed no-flight [skipped unavailable]) []
                [:no-dispatch-claim :no-dispatch-claim] :unidentified]
               [(with-completed failure []) [15] [] :matching-acknowledgment-recorded]
               [(with-completed no-flight []) [] [] :unidentified]]]
    (with-open [node (xtn/start-node {})]
      (submit-history! node (:transactions r))
      (doseq [[envelope times expected status] cases]
        (let [points (mapv #(vector (time-at %) (time-at %)) times)
              p (reference-reconciliation/predict-inspection-with-reads prior "lifecycle" envelope points)
              reads (atom [])
              expected-ids (cond-> (mapv :intent-id (filter :status (:completed envelope)))
                             (:in-flight envelope) (conj (get-in envelope [:in-flight :intent-id])))
              actual (with-redefs [frontier/q-from! (fn [_ [_ intent-id :as query]]
                                                     (let [[s v] (nth points (count @reads))]
                                                       (swap! reads conj intent-id)
                                                       ((pinned-read node s v) node query)))]
                       (reconciliation/inspect! node "lifecycle" envelope))]
          (is (= :predicted (:status p)))
          (is (= expected (mapv :status (:completed-observations actual))))
          (is (= status (:status actual)))
          (is (= (:observation p) actual))
          (is (= expected-ids @reads))
          (is (true? (:rescan-required? actual)))
          (is (= :none (:authority p)))
          (is (= :not-issued (:certificate p)))
          (is (not (contains? actual :may-replay?)))))
      (let [valid (with-completed no-flight [a b])
            invalids [(assoc valid :completed-count 1)
                      (assoc valid :last-completed-cursor (id 1))
                      (assoc valid :completed [b a])
                      (assoc valid :completed [a a])
                      (assoc valid :completed (vec (repeat 129 a)))
                      (assoc valid :completed [a (dissoc b :progression)])
                      (assoc valid :completed [a (assoc b :worker-id " ")])
                      (assoc valid :completed [a (assoc b :receipt-id "bad")])
                      (assoc valid :completed [a (assoc b :acquisition-status :contended)])
                      (assoc valid :completed [a (assoc unavailable :lease-id (id 11))])
                      (assoc valid :completed :not-a-vector)]
            points (vec (repeat 2 [(time-at 15) (time-at 15)]))]
        (doseq [envelope invalids]
          (let [p (reference-reconciliation/predict-inspection-with-reads prior "lifecycle" envelope points)
                reads (atom 0)
                rejected (with-redefs [frontier/q-from! (fn [& _] (swap! reads inc)
                                                          (throw (ex-info "Unexpected read" {})))]
                           (try (reconciliation/inspect! node "lifecycle" envelope)
                                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
            (is (= :invalid-completed-prefix rejected))
            (is (zero? @reads))
            (is (= :unsupported (:status p)))
            (is (not (contains? p :observation)))))
        (doseq [bad-points [[] (pop points) (conj points (first points))
                            [[(time-at 15)] (second points)]
                            [[(time-at 16) (time-at 16)] (second points)]]]
          (let [p (reference-reconciliation/predict-inspection-with-reads prior "lifecycle" valid bad-points)]
            (is (= :unsupported (:status p)))
            (is (not (contains? p :observation)))))
        (is (= (reference-reconciliation/predict-inspection-with-reads prior "lifecycle" valid points)
               (reference-reconciliation/predict-inspection prior (time-at 15) (time-at 15) "lifecycle" valid)))))))
