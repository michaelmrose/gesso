(ns gesso.assurance.reference-cycle-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-cycle :as reference]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.worker :as worker]
            [gesso.live.outbox.delivery :as delivery]))

(defn- id [n] (java.util.UUID. 0 n))
(def ^:private source "reference-cycle")
(def ^:private opts {:worker-id "w" :max-items 4})
(defn- candidate [n status more?]
  {:kind :candidate :more? more?
   :acquisition (cond-> {:intent-id (id n) :status status}
                  (#{:acquired :reclaimed} status)
                  (assoc :lease-id (id (+ n 100)) :worker-id "w" :progression [:basis n])
                  (= :unavailable status) (assoc :reason :not-pending))})
(defn- delivered [n status more? kind]
  (assoc (candidate n status more?) :delivery {:kind kind :receipt-id (id (+ n 200))}))
(defn- run-production [options steps]
  (let [remaining (atom steps) current (atom nil) calls (atom [])
        run (fn []
              (with-redefs [worker/acquire-sweep!
                            (fn [_ request]
                              (swap! calls conj [:acquire request])
                              (let [step (first @remaining) a (:acquisition step)]
                                (swap! remaining #(vec (rest %)))
                                (reset! current step)
                                (case (:kind step)
                                  :end {:results [] :attempted 0 :halt-reason :observed-end :next-after nil}
                                  :acquisition-error (throw (ex-info "unknown acquisition" {:error/type :test/acquisition-unknown}))
                                  :raw-page (:page step)
                                  :candidate {:results [a] :attempted 1
                                              :halt-reason (if (:more? step) :page-budget :observed-end)
                                              :next-after (when (:more? step) (:intent-id a))}
                                  (throw (AssertionError. "unexpected sweep")))))
                            delivery/dispatch-acquired!
                            (fn [_ _ _ a _]
                              (swap! calls conj [:dispatch a])
                              (let [{:keys [kind receipt-id]} (:delivery @current)
                                    ids (assoc (select-keys a [:intent-id :lease-id]) :receipt-id receipt-id)]
                                (if (= :raw-page (:kind @current))
                                  (:delivery-report @current)
                                  (case kind
                                  :accepted (assoc ids :status :dispatch-accepted :progression (:progression a)
                                                   :source-result {:status :emitted-many :count 1 :results [{:status :emitted}]}
                                                   :acknowledgment (assoc ids :status :acknowledged))
                                  :contended (assoc ids :status :dispatched-unacknowledged :acknowledgment {:status :contended})
                                  :unknown (throw (ex-info "lost response" (assoc ids :error/type :gesso.live.outbox/post-dispatch-ack-unknown
                                                                                :dispatch/status :accepted :acknowledgment/status :unknown)))
                                  (throw (AssertionError. "unexpected dispatch"))))))]
                (cycle/process-cycle! :node {:source :stub} {} source options)))
        outcome (try {:result (run)} (catch clojure.lang.ExceptionInfo e {:failure (ex-data e)}))]
    (assoc outcome :calls @calls)))

(deftest independent-cycle-control-flow
  (let [success (delivered 1 :acquired true :accepted)
        contended (candidate 2 :contended true)
        reclaimed (delivered 3 :reclaimed true :accepted)
        unavailable (candidate 4 :unavailable false)
        cases [[opts [{:kind :end}] :observed-end 0 1]
               [opts [success contended reclaimed unavailable] :observed-end 4 6]
               [(assoc opts :max-items 2) [success contended reclaimed] :page-budget 2 3]
               [(assoc opts :after (id 2)) [reclaimed {:kind :end}] :observed-end 1 3]
               [opts [success {:kind :acquisition-error} reclaimed] :acquisition 1 3]
               [opts [success (delivered 2 :reclaimed true :contended) reclaimed] :acknowledgment 1 4]
               [opts [success (delivered 2 :acquired true :unknown) reclaimed] :delivery 1 4]]]
    (doseq [[options steps halt completed calls] cases]
      (let [p (reference/predict-cycle source options steps) actual (run-production options steps)]
        (is (= :predicted (:status p)))
        (is (= :none (:authority p)))
        (is (= :not-issued (:certificate p)))
        (is (= actual (select-keys p [:result :failure :calls])))
        (is (= calls (count (:calls p))))
        (if-let [failure (:failure p)]
          (do (is (= halt (:phase failure)))
              (is (= completed (:completed-count failure)))
              (is (= (id 1) (:last-completed-cursor failure)))
              (is (true? (:rescan-required? failure))))
          (do (is (= halt (get-in p [:result :halt-reason])))
              (is (= completed (get-in p [:result :processed])))))))))

(deftest unsupported-cycle-hypotheses-have-no-partial-report
  (doseq [[options steps] [[(assoc opts :max-items 0) [{:kind :end}]]
                           [opts []]
                           [opts [(candidate 1 :contended true)]]
                           [opts [(candidate 2 :contended true) (candidate 1 :unavailable false)]]
                           [opts [(assoc-in (delivered 1 :acquired false :accepted) [:acquisition :worker-id] "other")]]
                           [opts [(delivered 1 :acquired false :invalid)]]]]
    (let [p (reference/predict-cycle source options steps)]
      (is (= :unsupported (:status p)))
      (is (not-any? #(contains? p %) [:result :failure :calls])))))

(defn- raw-page [a]
  {:kind :raw-page :page {:results [a] :attempted 1 :halt-reason :page-budget :next-after (:intent-id a)}})

(defn- raw-positive [a]
  (let [ids (assoc (select-keys a [:intent-id :lease-id]) :receipt-id (id 302))]
    (assoc ids :status :dispatch-accepted :progression (:progression a)
           :acknowledgment (assoc ids :status :acknowledged)
           :source-result {:status :emitted-many :count 2 :results [{:status :emitted} {:status :queued}]})))

(deftest selected-raw-worker-page-rejection
  (let [first-step (candidate 1 :contended true)
        a (:acquisition (candidate 2 :acquired true)) page (raw-page a)
        later (candidate 3 :unavailable false)
        cases [[(assoc-in page [:page :attempted] 0) true]
               [(assoc-in page [:page :next-after] (id 3)) true]
               [(assoc-in page [:page :halt-reason] :observed-end) true]
               [(assoc-in page [:page :halt-reason] :unknown) true]
               [(assoc-in page [:page :results 0 :worker-id] "foreign") true]
               [(assoc-in page [:page :results 0 :intent-id] (id 1)) true]
               [(assoc-in page [:page :results 0 :lease-id] "invalid") false]
               [(assoc-in page [:page :results 0 :intent-id] nil) false]
               [(assoc-in page [:page :results] [a a]) false]
               [(assoc-in page [:page :results] (list a)) false]
               [{:kind :raw-page :page nil} false]]]
    (doseq [[step evidence?] cases]
      (let [trace [first-step step later] p (reference/predict-cycle source opts trace)
            f (:failure p)]
        (is (= :predicted (:status p)))
        (is (= (run-production opts trace) (select-keys p [:failure :calls])))
        (is (= :acquisition-exception (:reason f)))
        (is (= :gesso.live.outbox/invalid-cycle (:original-error/type f)))
        (is (= 1 (:completed-count f)))
        (is (= (id 1) (:last-completed-cursor f)))
        (is (= 2 (count (:calls p))))
        (is (= evidence? (= :unverified-worker-page (get-in f [:in-flight :evidence]))))
        (is (true? (:rescan-required? f)))))
    (let [trace [first-step (raw-page (assoc a :status :unexpected)) later]
          p (reference/predict-cycle source opts trace)]
      (is (= (run-production opts trace) (select-keys p [:failure :calls])))
      (is (= :unexpected-acquisition-status (get-in p [:failure :reason])))
      (is (= 2 (count (:calls p)))))))

(deftest selected-raw-delivery-rejection
  (let [first-step (candidate 1 :contended true)
        a (:acquisition (candidate 2 :reclaimed true)) page (raw-page a)
        d (raw-positive a) later (candidate 3 :unavailable false)
        cases [[(assoc d :intent-id (id 9)) false :invalid-delivery-result]
               [(assoc d :lease-id (id 9)) false :invalid-delivery-result]
               [(assoc d :receipt-id "invalid") false :invalid-delivery-result]
               [(assoc d :progression [:foreign]) true :invalid-delivery-result]
               [(assoc-in d [:acknowledgment :receipt-id] (id 9)) true :invalid-delivery-result]
               [(assoc-in d [:acknowledgment :status] :contended) true :invalid-delivery-result]
               [(assoc-in d [:source-result :count] 1) true :invalid-delivery-result]
               [(assoc-in d [:source-result :results 1 :status] :rejected) true :invalid-delivery-result]
               [(assoc d :source-result {:status :emitted-many :count 0 :results []}) true :invalid-delivery-result]
               [(assoc d :status :dispatched-unacknowledged :acknowledgment {:status :unavailable}) true :not-dispatch-accepted]
               [(assoc d :status :dispatched-unacknowledged :acknowledgment {:status :unknown}) false :not-dispatch-accepted]
               [(assoc d :status :failed) false :not-dispatch-accepted]
               [nil false :not-dispatch-accepted]]]
    (doseq [[report evidence? reason] cases]
      (let [trace [first-step (assoc page :delivery-report report) later]
            p (reference/predict-cycle source opts trace) f (:failure p)]
        (is (= :predicted (:status p)))
        (is (= (run-production opts trace) (select-keys p [:failure :calls])))
        (is (= reason (:reason f)))
        (is (= 1 (:completed-count f)))
        (is (= (id 1) (:last-completed-cursor f)))
        (is (= 3 (count (:calls p))))
        (is (= evidence? (= :unverified-post-dispatch-receipt (get-in f [:in-flight :receipt-evidence]))))
        (is (= (when evidence? (:receipt-id d)) (get-in f [:in-flight :receipt-id])))
        (is (= :none (:authority p)))))
    (let [trace [first-step (assoc page :delivery-report d) later]
          p (reference/predict-cycle source opts trace)]
      (is (= (run-production opts trace) (select-keys p [:result :calls])))
      (is (= 3 (get-in p [:result :processed])))
      (is (= 1 (get-in p [:result :dispatch-accepted]))))))
