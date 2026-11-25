(ns gesso.assurance.reference-physical-cycle
  "Selected composition of physical replay and bounded cycle control flow."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-cycle :as cycle]
            [gesso.assurance.reference-discovery :as discovery]
            [gesso.assurance.reference-reconciliation :as reconciliation]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant)))

(def profile :selected-physical-cycle-v3)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})
(def ^:private unsupported (assoc base :status :unsupported))
(defn- time? [x] (and (instance? Instant x) (zero? (mod (.getNano ^Instant x) 1000))))
(defn- before? [^Instant a ^Instant b] (.isBefore a b))
(defn- latest [r] (:system-time (peek (:history r))))
(defn- slot? [s]
  (and (map? s) (nil? (meta s))
       (= #{:scan-time :acquire-time :ack-time :lease-id :receipt-id} (disj (set (keys s)) :ack-response))
       (#{:observed :lost-before-submit :lost-after-submit} (get s :ack-response :observed))
       (every? time? (map s [:scan-time :acquire-time :ack-time]))
       (before? (:scan-time s) (:acquire-time s)) (before? (:acquire-time s) (:ack-time s))
       (uuid? (:lease-id s)) (uuid? (:receipt-id s))))
(defn- append-tx [prefix time p]
  (cond-> prefix (seq (:ops p)) (conj {:system-time time :ops (:ops p)})))

(defn- attempt [prefix prior source opts slot page]
  (let [{:keys [intent-id action]} (first (:candidates page))
        {:keys [scan-time acquire-time ack-time lease-id receipt-id ack-response]} slot
        worker (:worker-id opts)
        deadline (.plusMillis ^Instant scan-time (long (get opts :lease-millis 60000)))
        claim {:intent-id intent-id :source-id source :worker-id worker :lease-id lease-id
               :now scan-time :lease-until deadline}
        p ((case action :acquire-pending temporal/predict-pending-acquisition
                 :reclaim-expired temporal/predict-expired-reclamation)
           prefix (latest prior) scan-time acquire-time claim)]
    (when (and (= :predicted (:status p)) (#{:acquired :reclaimed :contended :unavailable} (:outcome p)))
      (let [prefix (append-tx prefix acquire-time p)
            positive? (#{:acquired :reclaimed} (:outcome p))
            a (cond-> {:intent-id intent-id :status (:outcome p) :action action}
                (:reason p) (assoc :reason (:reason p))
                positive? (assoc :lease-id lease-id :worker-id worker :lease-until deadline
                                 :progression [:selected-acquisition acquire-time]))
            step {:kind :candidate :more? (:has-more? page) :acquisition a}]
        (if positive?
          (let [submission {:intent-id intent-id :lease-id lease-id :worker-id worker
                            :receipt {:receipt-id receipt-id :adapter :test/adapter :result :accepted}}
                ack (temporal/predict-acknowledgment prefix (latest (:replay p)) ack-time ack-time submission)]
            (when (and (= :predicted (:status ack)) (#{:acknowledged :contended} (:outcome ack)))
              (let [lost? (#{:lost-before-submit :lost-after-submit} ack-response)
                    submitted? (not= :lost-before-submit ack-response)]
                {:prefix (if submitted? (append-tx prefix ack-time ack) prefix)
                 :replay (if submitted? (:replay ack) (:replay p))
                 :step (assoc step :delivery {:kind (cond lost? :unknown
                                                         (= :acknowledged (:outcome ack)) :accepted
                                                         :else :contended)
                                              :receipt-id receipt-id})
                 :stop? (or lost? (= :contended (:outcome ack)))})))
          {:prefix prefix :replay (:replay p) :step step})))))

(defn predict-physical-cycle
  "Compose one-candidate discovery, physical acquisition and selected trusted
   acceptance/acknowledgment with independent cycle summaries. Each reached
   sweep consumes a clock/identity slot; no outcome or row readback is input.
   The progression marker represents only the supplied acquisition time, not
   a provider token. No concurrent writers, actual dispatch, live frontier,
   universal refinement is established. Selected lost responses are
   physical hypotheses, not information available to the interrupted worker."
  [prefix source opts slots]
  (let [prior (temporal/replay prefix)
        budget (get opts :max-items 32)]
    (if-not (and (= :predicted (:status prior)) (seq (:history prior))
                 (string? source) (not (str/blank? source))
                 (map? opts) (nil? (meta opts))
                 (= :predicted (:status (cycle/predict-cycle source opts [{:kind :end}])))
                 (vector? slots) (nil? (meta slots)) (<= 1 (count slots) 128)
                 (every? slot? slots)
                 (before? (latest prior) (:scan-time (first slots)))
                 (every? (fn [[a b]] (before? (:ack-time a) (:scan-time b))) (partition 2 1 slots))
                 (= (count slots) (count (distinct (map :lease-id slots)))))
      unsupported
      (loop [prefix prefix prior prior cursor (:after opts) remaining budget todo slots pages [] steps []]
        (if-let [slot (first todo)]
          (let [request (cond-> {:source-id source :limit 1} cursor (assoc :after cursor))
                d (discovery/predict-page prior (:scan-time slot) request)
                page (:page d)
                pages (conj pages page)
                finish (fn [prefix prior steps]
                         (let [c (cycle/predict-cycle source opts steps)]
                           (if (= :predicted (:status c))
                             (assoc base :status :predicted :pages pages :cycle c
                                    :transactions prefix :replay prior)
                             unsupported)))]
            (cond
              (not= :predicted (:status d)) unsupported
              (empty? (:candidates page)) (finish prefix prior (conj steps {:kind :end}))
              :else (if-let [a (attempt prefix prior source opts slot page)]
                      (let [steps (conj steps (:step a))]
                        (if (or (:stop? a) (not (:has-more? page)) (= remaining 1))
                          (finish (:prefix a) (:replay a) steps)
                          (recur (:prefix a) (:replay a) (:next-after page) (dec remaining)
                                 (next todo) pages steps)))
                      unsupported)))
          unsupported)))))

(defn predict-interrupted-inspection
  "Compose the selected cycle failure with read-only reconciliation. Project
   only reconciliation-relevant fields of the independently predicted cycle
   envelope; acquisition-status is diagnostic, not an ownership credential.
   The supplied physical history hypothesis and pinned point do not resolve
   a genuinely outstanding transaction or authorize restart."
  [physical source system-time valid-time]
  (let [failure (get-in physical [:cycle :failure])]
    (if (and (= profile (:profile physical)) (= :predicted (:status physical))
             (= :delivery (:phase failure)) (= :delivery-exception (:reason failure))
             (= :gesso.live.outbox/post-dispatch-ack-unknown (:original-error/type failure)))
      (reconciliation/predict-inspection
       (:replay physical) system-time valid-time source
       (-> (select-keys failure [:error/type :rescan-required? :completed
                                :completed-count :last-completed-cursor :in-flight])
           (update :in-flight dissoc :acquisition-status)))
      unsupported)))

(defn predict-recovery-trajectory
  "Compose one interrupted cycle, pinned inspection and a fresh cycle.
   Restart is an explicit hypothetical caller choice, not derived authority.
   The recovery scan starts without a cursor and uses distinct supplied lease
   identities. Its physical prefix comes only from the first prediction."
  [prefix source initial-opts initial-slots inspection-time recovery-opts recovery-slots]
  (let [initial (predict-physical-cycle prefix source initial-opts initial-slots)
        reached (count (:pages initial))]
    (if-not (and (= :predicted (:status initial)) (pos? reached)
                 (time? inspection-time)
                 (not (before? inspection-time (:ack-time (nth initial-slots (dec reached)))))
                 (map? recovery-opts) (not (contains? recovery-opts :after))
                 (vector? recovery-slots) (seq recovery-slots) (every? slot? recovery-slots)
                 (every? #(= :observed (get % :ack-response :observed)) recovery-slots)
                 (before? inspection-time (:scan-time (first recovery-slots)))
                 (not-any? (set (map :lease-id initial-slots)) (map :lease-id recovery-slots)))
      unsupported
      (let [inspection (predict-interrupted-inspection initial source (latest (:replay initial)) inspection-time)
            recovery (predict-physical-cycle (:transactions initial) source recovery-opts recovery-slots)]
        (if (and (= :predicted (:status inspection)) (= :predicted (:status recovery)))
          (assoc base :status :predicted :initial initial :inspection (:observation inspection)
                 :recovery recovery :transactions (:transactions recovery) :replay (:replay recovery))
          unsupported)))))
