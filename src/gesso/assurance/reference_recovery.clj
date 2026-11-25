(ns gesso.assurance.reference-recovery
  "Compose selected read-only reconciliation, fresh discovery and recovery."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-discovery :as discovery]
            [gesso.assurance.reference-reconciliation :as reconciliation]
            [gesso.assurance.reference-temporal :as temporal])
  (:import (java.time Instant)))

(def profile :selected-worker-recovery-v1)
(def ^:private base-report {:profile profile :authority :none :certificate :not-issued})
(def ^:private unsupported (assoc base-report :status :unsupported))
(defn- time? [x]
  (and (instance? Instant x) (zero? (mod (.getNano ^Instant x) 1000))))
(defn- before? [a b] (.isBefore ^Instant a ^Instant b))
(defn- latest [r] (:system-time (peek (:history r))))
(defn- slot? [slot]
  (and (map? slot) (nil? (meta slot))
       (= #{:worker-id :lease-id :now :lease-until :acquire-time :ack-time :receipt-id :adapter}
          (set (keys slot)))
       (string? (:worker-id slot)) (not (str/blank? (:worker-id slot)))
       (uuid? (:lease-id slot)) (uuid? (:receipt-id slot)) (qualified-keyword? (:adapter slot))
       (every? time? (map slot [:now :lease-until :acquire-time :ack-time]))
       (not (before? (:acquire-time slot) (:now slot)))
       (before? (:now slot) (:lease-until slot))
       (before? (:acquire-time slot) (:ack-time slot))))
(defn- extend-prefix [prefix time p]
  (cond-> prefix (seq (:ops p)) (conj {:system-time time :ops (:ops p)})))
(defn- call [kind s v time input p]
  (assoc (select-keys p [:outcome :reason :ops])
         :kind kind :observed-system s :observed-valid v :system-time time :input input))

(defn- recover-one [prefix prior source candidate slot]
  (let [{:keys [intent-id action]} candidate
        {:keys [worker-id lease-id now lease-until acquire-time ack-time receipt-id adapter]} slot
        claim {:intent-id intent-id :source-id source :worker-id worker-id
               :lease-id lease-id :now now :lease-until lease-until}
        s (latest prior)
        p ((case action :acquire-pending temporal/predict-pending-acquisition
                 :reclaim-expired temporal/predict-expired-reclamation)
           prefix s acquire-time acquire-time claim)]
    (when (and (= :predicted (:status p)) (#{:acquired :reclaimed :contended :unavailable} (:outcome p)))
      (let [prefix (extend-prefix prefix acquire-time p)
            calls [(call action s acquire-time acquire-time claim p)]]
        (if (#{:acquired :reclaimed} (:outcome p))
          (let [submission {:intent-id intent-id :worker-id worker-id :lease-id lease-id
                            :receipt {:receipt-id receipt-id :adapter adapter :result :accepted}}
                s (latest (:replay p))
                a (temporal/predict-acknowledgment prefix s ack-time ack-time submission)]
            (when (= :predicted (:status a))
              {:prefix (extend-prefix prefix ack-time a) :replay (:replay a)
               :calls (conj calls (call :acknowledge s ack-time ack-time submission a))}))
          {:prefix prefix :replay (:replay p) :calls calls})))))

(defn predict-recovery
  "Predict one fresh, complete bounded page from a supplied physical history
   hypothesis. Reconciliation is diagnostic, not an authorization to retry.
   Candidate IDs/actions come from independent discovery, not the interrupted
   report. Slots supply explicit identities, clocks and trusted receipts;
   acknowledgment is attempted only after a successful physical acquisition.
   No concurrent writer, live frontier, multipage cycle, adapter or actual
   restart permission is established. Unsupported inputs yield no partial trace."
  [prefix source failure scan-time slots]
  (let [prior (temporal/replay prefix)
        s (latest prior)]
    (if-not (and (= :predicted (:status prior)) (time? scan-time)
                 (not (before? scan-time s))
                 (vector? slots) (nil? (meta slots)) (<= (count slots) 128)
                 (every? slot? slots)
                 (every? #(not (before? (:now %) scan-time)) slots)
                 (every? (fn [[a b]] (before? (:ack-time a) (:acquire-time b))) (partition 2 1 slots))
                 (or (empty? slots) (before? scan-time (:acquire-time (first slots))))
                 (= (count slots) (count (distinct (map :lease-id slots)))))
      unsupported
      (let [inspection (reconciliation/predict-inspection prior s scan-time source failure)
            discovery (discovery/predict-page prior scan-time {:source-id source :limit 128})
            page (:page discovery)]
        (if-not (and (= :predicted (:status inspection)) (= :predicted (:status discovery))
                     (false? (:has-more? page)) (= (count slots) (count (:candidates page))))
          unsupported
          (loop [items (seq (map vector (:candidates page) slots)) prefix prefix prior prior calls []]
            (if-let [[candidate slot] (first items)]
              (if-let [r (recover-one prefix prior source candidate slot)]
                (recur (next items) (:prefix r) (:replay r) (into calls (:calls r)))
                unsupported)
              (assoc base-report :status :predicted :inspection (:observation inspection)
                     :page page :calls calls :transactions prefix :replay prior))))))))
