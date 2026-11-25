(ns gesso.assurance.reference-physical-cycle-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-physical-cycle :as reference]
            [gesso.assurance.reference-lifecycle :as lifecycle]
            [gesso.assurance.reference-discovery :as reference-discovery]
            [gesso.assurance.reference-temporal-fixtures :refer [sys provider-rectangles coalesce-valid]]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.outbox.reconciliation :as reconciliation]
            [gesso.live.outbox.worker :as worker]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import (java.time Instant)))

(defn- id [n] (java.util.UUID. 0 n))
(defn- time-at [n] (.plusSeconds ^Instant (sys 1) n))
(def ^:private source "physical-cycle")
(def ^:private opts {:worker-id "cycle-worker" :lease-millis 10000 :max-items 3})
(defn- publish [n source ids]
  {:kind :publish :system-time (time-at n)
   :spec {:source-id source :intent-ids ids :operation :request/claim :purpose :gesso.live/refresh}
   :changes (mapv (fn [i] {:topic :request :id (str i)}) ids)
   :writes [[:put-docs :cycleprobe {:xt/id source :value n}]]})
(def ^:private seed
  [(publish 0 source [(id 1) (id 2) (id 3)])
   (publish 1 "foreign" [(id 4)])
   {:kind :acquire :system-time (time-at 2) :observed-system (time-at 1) :observed-valid (time-at 2)
    :claim {:intent-id (id 1) :lease-id (id 10) :worker-id "old" :source-id source
            :now (time-at 2) :lease-until (time-at 10)}}])
(defn- slot [scan acquire ack n]
  {:scan-time (time-at scan) :acquire-time (time-at acquire) :ack-time (time-at ack)
   :lease-id (id (+ 100 n)) :receipt-id (id (+ 200 n))})
(def ^:private ordinary-slots [(slot 10 11 12 1) (slot 13 14 15 2) (slot 16 17 18 3)])

(defn- run-physical-cycle! [node prefix options slots]
  ;; Actual cycle and sweep orchestration, including their page validation.
  ;; Substitute deterministic host claim generation and trusted acceptance
  ;; at the private attempt / delivery boundaries. Actual discovery SQL,
  ;; acquisition, acknowledgment and transaction adapter are not stubbed.
  (let [sweep! worker/acquire-sweep! discover! discovery/discover-eligible!
        execute! live.xtdb/execute-tx!
        slots (atom (seq slots)) current (atom nil) phase (atom :discovery)
        last-system (atom (:system-time (peek prefix)))
        calls (atom []) pages (atom []) transactions (atom [])
        lost (ex-info "Lost physical acknowledgment response" {:test/outcome :unknown})
        attempt! (fn [node worker-id source-id lease-millis {:keys [intent-id action]}]
                   (reset! phase :acquisition)
                   (let [{:keys [scan-time lease-id]} @current
                         claim {:intent-id intent-id :worker-id worker-id :source-id source-id
                                :lease-id lease-id :now scan-time :lease-until (.plusMillis ^Instant scan-time lease-millis)}
                         actual ((case action :acquire-pending acquisition/acquire-pending!
                                       :reclaim-expired acquisition/reclaim-expired!) node claim)
                         actual (#'worker/validate-acquisition-result! claim action actual)]
                     ;; Project only the issuing system time; generic progression
                     ;; token construction/validation is a separate obligation.
                     (cond-> (assoc (select-keys actual [:intent-id :status :reason :lease-id :worker-id :lease-until])
                                    :action action)
                       (#{:acquired :reclaimed} (:status actual))
                       (assoc :progression (do (is (some? (:progression actual)))
                                               [:selected-acquisition (get-in actual [:consistency :system-time])])))))
        delivered! (fn [node _system _ctx a _source]
                     (swap! calls conj [:dispatch a])
                     (reset! phase :acknowledgment)
                     (let [ids (assoc (select-keys a [:intent-id :lease-id]) :receipt-id (:receipt-id @current))
                           ack (try
                                 (acknowledgment/acknowledge-dispatch! node
                                   (assoc (select-keys a [:intent-id :lease-id :worker-id])
                                          :receipt {:receipt-id (:receipt-id @current) :adapter :test/adapter :result :accepted}))
                                 (catch Throwable e
                                   (if (identical? lost e)
                                     (throw (ex-info "Controlled post-dispatch response loss"
                                                     (assoc ids :error/type :gesso.live.outbox/post-dispatch-ack-unknown
                                                            :dispatch/status :accepted :acknowledgment/status :unknown) e))
                                     (throw e))))]
                       (if (= :acknowledged (:status ack))
                         (assoc ids :status :dispatch-accepted :progression (:progression a)
                                :source-result {:status :emitted-many :count 1 :results [{:status :emitted}]}
                                :acknowledgment ack)
                         (assoc ids :status :dispatched-unacknowledged :acknowledgment ack))))
        result (with-redefs-fn
                 {#'worker/attempt! attempt!
                  #'worker/acquire-sweep! (fn [node request]
                                           (swap! calls conj [:acquire request])
                                           (when-not (seq @slots) (throw (AssertionError. "Unexpected extra sweep")))
                                           (reset! current (first @slots))
                                           (swap! slots next)
                                           (reset! phase :discovery)
                                           (sweep! node request))
                  #'discovery/discover-eligible! (fn [node request]
                                                 (let [page (discover! node request)]
                                                   (swap! pages conj page) page))
                  #'delivery/dispatch-acquired! delivered!
                  #'frontier/q-from! (fn [node query]
                                       (if (= :discovery @phase)
                                         (xt/q node query {:current-time (:scan-time @current)})
                                         (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?"
                                                     (if (= :acquisition @phase) (:scan-time @current) (:ack-time @current))
                                                     @last-system (second query)])))
                  #'live.xtdb/execute-tx! (fn [node ops]
                                           (let [time (if (= :acquisition @phase) (:acquire-time @current) (:ack-time @current))
                                                 response (when (= :acknowledgment @phase) (:ack-response @current))]
                                             (when (= :lost-before-submit response) (throw lost))
                                             (swap! transactions conj {:system-time time :ops ops})
                                             (reset! last-system time)
                                             (if (= :lost-after-submit response)
                                               (do (try (execute! node ops {:system-time time})
                                                        (catch Throwable e
                                                          (when-not (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                                                            (throw e))))
                                                   (throw lost))
                                               (execute! node ops {:system-time time}))))}
                 (fn []
                   (try {:result (cycle/process-cycle! node {:source :controlled} {} source options)}
                        (catch clojure.lang.ExceptionInfo e
                          (if (= :gesso.live.outbox/cycle-interrupted (:error/type (ex-data e)))
                            {:failure (ex-data e)} (throw e))))))]
    (assoc result :calls @calls :pages @pages :transactions @transactions)))

(deftest independently-composed-physical-worker-cycle
  (let [history (lifecycle/predict-lifecycle seed) prefix (:transactions history)]
    (is (= :predicted (:status history)))
    (doseq [[options slots expected processed submissions]
            [[opts ordinary-slots :observed-end 3 6]
             [(assoc opts :max-items 1) ordinary-slots :page-budget 1 2]
             [(assoc opts :after (id 1)) ordinary-slots :observed-end 2 4]
             [opts [(slot 10 20 21 1) (slot 22 23 24 2) (slot 25 26 27 3)] :observed-end 3 5]
             [opts [(slot 10 11 12 1) (slot 13 14 23 2) (slot 24 25 26 3)] :acknowledgment 1 4]
             [(assoc opts :after (id 3)) ordinary-slots :observed-end 0 0]]]
      (let [p (reference/predict-physical-cycle prefix source options slots)
            c (:cycle p)]
        (is (= :predicted (:status p)))
        (is (= :none (:authority p)))
        (is (= :not-issued (:certificate p)))
        (is (= expected (or (get-in c [:failure :phase]) (get-in c [:result :halt-reason]))))
        (is (= processed (or (get-in c [:failure :completed-count]) (get-in c [:result :processed]))))
        (is (= submissions (- (count (:transactions p)) (count prefix))))
        (with-open [node (xtn/start-node {})]
          (doseq [{:keys [system-time ops]} prefix]
            (live.xtdb/execute-tx! node ops {:system-time system-time}))
          (let [actual (run-physical-cycle! node prefix options slots)]
            (is (= (select-keys c [:result :failure :calls]) (select-keys actual [:result :failure :calls])))
            (is (= (:pages p) (:pages actual)))
            (is (= (subvec (:transactions p) (count prefix)) (:transactions actual)))
            (is (= (frequencies (coalesce-valid (get-in p [:replay :rectangles])))
                   (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) [:gesso_live_outbox :cycleprobe])))))
            (let [clock (time-at 40) request {:source-id source :limit 8}
                  actual-page (with-redefs [frontier/q-from! (fn [node query] (xt/q node query {:current-time clock}))]
                                (discovery/discover-eligible! node request))]
              (is (= actual-page (:page (reference-discovery/predict-page (:replay p) clock request))))
              (is (= (cond (= expected :acknowledgment) [(id 2) (id 3)]
                           (= :page-budget expected) [(id 2) (id 3)]
                           (= (id 1) (:after options)) [(id 1)]
                           (= (id 3) (:after options)) [(id 1) (id 2) (id 3)]
                           (= 5 submissions) [(id 1)]
                           :else []) (mapv :intent-id (:candidates actual-page)))))))))))

(deftest unsupported-physical-cycle-plans-return-no-partial-trace
  (let [prefix (:transactions (lifecycle/predict-lifecycle seed))]
    (doseq [[options slots] [[opts []]
                             [opts (subvec ordinary-slots 0 1)]
                             [opts (assoc-in ordinary-slots [1 :lease-id] (id 101))]
                             [opts (assoc-in ordinary-slots [0 :lease-id] (id 10))]
                             [opts (assoc-in ordinary-slots [0 :scan-time] (time-at 2))]
                             [opts (assoc-in ordinary-slots [1 :scan-time] (time-at 12))]
                             [(assoc opts :lease-millis 0) ordinary-slots]
                             [opts (assoc-in ordinary-slots [1 :extra] true)]]]
      (let [p (reference/predict-physical-cycle prefix source options slots)]
        (is (= :unsupported (:status p)))
        (is (not-any? #(contains? p %) [:pages :cycle :transactions :replay]))))))

(deftest uncertain-physical-cycle-and-reconciliation
  (let [prefix (:transactions (lifecycle/predict-lifecycle seed))
        failures (atom [])]
    (doseq [[response ack-time expected-submissions expected-disposition eligible]
            [[:lost-before-submit 15 3 :same-lease-active [(id 2) (id 3)]]
             [:lost-after-submit 15 4 :matching-acknowledgment-recorded [(id 3)]]
             [:lost-after-submit 23 4 :same-lease-active [(id 2) (id 3)]]]]
      (let [slots [(first ordinary-slots)
                   (assoc (slot 13 14 ack-time 2) :ack-response response)
                   (slot 24 25 26 3)]
            p (reference/predict-physical-cycle prefix source opts slots)
            c (:cycle p) failure (:failure c)
            s (:system-time (peek (:transactions p))) v (time-at 40)
            inspection (reference/predict-interrupted-inspection p source s v)]
        (is (= :predicted (:status p)))
        (is (= :delivery (:phase failure)))
        (is (= 1 (:completed-count failure)))
        (is (= (id 1) (:last-completed-cursor failure)))
        (is (= (id 202) (get-in failure [:in-flight :receipt-id])))
        (is (= :unverified-post-dispatch-receipt (get-in failure [:in-flight :receipt-evidence])))
        (is (= expected-submissions (- (count (:transactions p)) (count prefix))))
        (is (= :predicted (:status inspection)))
        (is (= :none (:authority inspection)))
        (is (= expected-disposition (get-in inspection [:observation :status])))
        (is (= [:matching-acknowledgment-recorded] (mapv :status (get-in inspection [:observation :completed-observations]))))
        (swap! failures conj failure)
        (with-open [node (xtn/start-node {})]
          (doseq [{:keys [system-time ops]} prefix]
            (live.xtdb/execute-tx! node ops {:system-time system-time}))
          (let [actual (run-physical-cycle! node prefix opts slots)
                reads (atom [])
                observed (with-redefs [frontier/q-from! (fn [node [_ id]]
                                                         (swap! reads conj id)
                                                         (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? WHERE _id = ?" v s id]))]
                           (reconciliation/inspect! node source (:failure actual)))
                page (with-redefs [frontier/q-from! (fn [node query] (xt/q node query {:current-time v}))]
                       (discovery/discover-eligible! node {:source-id source :limit 8}))]
            (is (= (select-keys c [:failure :calls]) (select-keys actual [:failure :calls])))
            (is (= (:pages p) (:pages actual)))
            (is (= (subvec (:transactions p) (count prefix)) (:transactions actual)))
            (is (= (:observation inspection) observed))
            (is (= [(id 1) (id 2)] @reads))
            (is (true? (:rescan-required? observed)))
            (is (= eligible (mapv :intent-id (:candidates page))))
            (is (= page (:page (reference-discovery/predict-page (:replay p) v {:source-id source :limit 8}))))
            (is (= (frequencies (coalesce-valid (get-in p [:replay :rectangles])))
                   (frequencies (coalesce-valid (mapcat #(provider-rectangles node %) [:gesso_live_outbox :cycleprobe])))))))))
    ;; Identical interruption evidence is compatible with distinct physical outcomes.
    (is (apply = @failures))
    (let [ordinary (reference/predict-physical-cycle prefix source opts ordinary-slots)]
      (is (= :unsupported (:status (reference/predict-interrupted-inspection ordinary source (time-at 18) (time-at 40))))))
    (is (= :unsupported (:status (reference/predict-physical-cycle prefix source opts
                                    (assoc-in ordinary-slots [1 :ack-response] :invalid)))))))
