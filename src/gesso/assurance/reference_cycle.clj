(ns gesso.assurance.reference-cycle
  "Selected cycle control flow over explicit adapter-report hypotheses."
  (:require [clojure.string :as str]))

(def profile :selected-worker-cycle-v1)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})
(def ^:private unsupported (assoc base :status :unsupported))
(defn- text? [x] (and (string? x) (not (str/blank? x))))
(defn- options? [source opts]
  (and (text? source) (map? opts) (nil? (meta opts))
       (every? #{:worker-id :max-items :lease-millis :after} (keys opts))
       (text? (:worker-id opts))
       (integer? (get opts :max-items 32)) (<= 1 (get opts :max-items 32) 128)
       (integer? (get opts :lease-millis 60000)) (<= 1 (get opts :lease-millis 60000) 300000)
       (or (not (contains? opts :after)) (uuid? (:after opts)))))

(defn- summary [opts results more? cursor]
  (let [frequencies (frequencies (map :acquisition-status results))]
    {:status :cycle-completed :worker-id (:worker-id opts) :processed (count results)
     :acquired (get frequencies :acquired 0) :reclaimed (get frequencies :reclaimed 0)
     :contended (get frequencies :contended 0) :unavailable (get frequencies :unavailable 0)
     :dispatch-accepted (count (filter #(= :dispatch-accepted (:status %)) results))
     :results results :halt-reason (if more? :page-budget :observed-end)
     :next-after (when more? cursor)}))

(defn- interrupted [results cursor in-flight phase reason details]
  (merge {:error/type :gesso.live.outbox/cycle-interrupted
          :phase phase :reason reason :completed results :completed-count (count results)
          :last-completed-cursor cursor :in-flight in-flight :rescan-required? true}
         details))

(defn- acquired? [a opts]
  (and (#{:acquired :reclaimed} (:status a)) (= (:worker-id opts) (:worker-id a))
       (uuid? (:lease-id a)) (some? (:progression a))))
(defn- entry? [a cursor opts]
  (and (map? a) (uuid? (:intent-id a))
       (or (nil? cursor) (neg? (compare (str cursor) (str (:intent-id a)))))
       (or (acquired? a opts) (#{:contended :unavailable} (:status a)))))

(defn- delivery-result [a d results cursor]
  (let [lease (assoc (select-keys a [:intent-id :lease-id :worker-id])
                     :acquisition-status (:status a))
        receipt (:receipt-id d)]
    (when (and (map? d) (= #{:kind :receipt-id} (set (keys d))) (uuid? receipt))
      (case (:kind d)
        :accepted {:result (assoc lease :status :dispatch-accepted :receipt-id receipt
                                  :progression (:progression a))}
        :contended {:failure (interrupted results cursor
                               (assoc lease :receipt-id receipt :receipt-evidence :unverified-post-dispatch-receipt)
                               :acknowledgment :not-dispatch-accepted
                               {:source-acceptance :reported :delivery-status :dispatched-unacknowledged
                                :acknowledgment-status :contended})}
        :unknown {:failure (interrupted results cursor
                             (assoc lease :receipt-id receipt :receipt-evidence :unverified-post-dispatch-receipt)
                             :delivery :delivery-exception
                             {:original-error/type :gesso.live.outbox/post-dispatch-ack-unknown
                              :source-acceptance :reported})}
        nil))))

(defn predict-cycle
  "Predict a bounded cycle over selected declarative adapter reports. Each
   reached step is an empty page, an acquisition exception without a known
   lease, or a single well-formed candidate with optional delivery outcome.
   Unreached steps are not consumed. No adapter truth, physical state,
   source delivery, live frontier or restart authority is established."
  [source opts steps]
  (if-not (and (options? source opts) (vector? steps) (nil? (meta steps)) (<= (count steps) 128))
    unsupported
    (loop [cursor (:after opts) remaining (get opts :max-items 32) results [] calls [] todo (seq steps)]
      (let [step (first todo)
            work (cond-> {:worker-id (:worker-id opts) :source-id source
                          :lease-millis (get opts :lease-millis 60000) :page-size 1 :max-pages 1}
                   cursor (assoc :after cursor))
            calls (conj calls [:acquire work])
            complete (fn [results more? cursor] (assoc base :status :predicted :calls calls
                                                       :result (summary opts results more? cursor)))]
        (cond
          (= step {:kind :end}) (complete results false nil)
          (= step {:kind :acquisition-error})
          (assoc base :status :predicted :calls calls
                 :failure (interrupted results cursor nil :acquisition :acquisition-exception
                                      {:original-error/type :test/acquisition-unknown}))
          (and (map? step) (= :candidate (:kind step)) (boolean? (:more? step))
               (entry? (:acquisition step) cursor opts))
          (let [a (:acquisition step) positive? (acquired? a opts)
                expected-keys (cond-> #{:kind :more? :acquisition} positive? (conj :delivery))
                calls (cond-> calls positive? (conj [:dispatch a]))
                attempt (when (= expected-keys (set (keys step)))
                          (if positive? (delivery-result a (:delivery step) results cursor)
                              {:result {:intent-id (:intent-id a) :acquisition-status (:status a)
                                        :reason (:reason a)}}))]
            (cond
              (nil? attempt) unsupported
              (:failure attempt) (assoc base :status :predicted :calls calls :failure (:failure attempt))
              :else (let [done (conj results (:result attempt)) more? (:more? step) id (:intent-id a)]
                      (if (and more? (> remaining 1))
                        (recur id (dec remaining) done calls (next todo))
                        (assoc base :status :predicted :calls calls :result (summary opts done more? id))))))
          :else unsupported)))))
