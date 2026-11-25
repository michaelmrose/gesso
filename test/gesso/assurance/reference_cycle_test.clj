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
                                  :candidate {:results [a] :attempted 1
                                              :halt-reason (if (:more? step) :page-budget :observed-end)
                                              :next-after (when (:more? step) (:intent-id a))}
                                  (throw (AssertionError. "unexpected sweep")))))
                            delivery/dispatch-acquired!
                            (fn [_ _ _ a _]
                              (swap! calls conj [:dispatch a])
                              (let [{:keys [kind receipt-id]} (:delivery @current)
                                    ids (assoc (select-keys a [:intent-id :lease-id]) :receipt-id receipt-id)]
                                (case kind
                                  :accepted (assoc ids :status :dispatch-accepted :progression (:progression a)
                                                   :source-result {:status :emitted-many :count 1 :results [{:status :emitted}]}
                                                   :acknowledgment (assoc ids :status :acknowledged))
                                  :contended (assoc ids :status :dispatched-unacknowledged :acknowledgment {:status :contended})
                                  :unknown (throw (ex-info "lost response" (assoc ids :error/type :gesso.live.outbox/post-dispatch-ack-unknown
                                                                                :dispatch/status :accepted :acknowledgment/status :unknown)))
                                  (throw (AssertionError. "unexpected dispatch")))))]
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
