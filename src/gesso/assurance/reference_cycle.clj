(ns gesso.assurance.reference-cycle
  "Selected cycle control flow over explicit adapter-report hypotheses."
  (:require [clojure.string :as str]))

(def profile :selected-worker-cycle-v2)
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

(defn- raw-page? [page]
  ;; Bound the fragment before trying to interpret malformed reports.
  (and (or (nil? page) (map? page))
       (let [rows (:results page)]
         (or (nil? rows)
             (and (or (vector? rows) (list? rows))
                  (<= (count rows) 128) (every? map? rows))))))

(defn- page-valid? [page cursor worker]
  (let [rows (:results page) a (first rows) id (:intent-id a)]
    (and (map? page) (vector? rows) (<= (count rows) 1)
         (= (count rows) (:attempted page))
         (or (empty? rows)
             (and (uuid? id)
                  (or (nil? cursor) (neg? (compare (str cursor) (str id))))
                  (or (not (#{:acquired :reclaimed} (:status a)))
                      (and (= worker (:worker-id a)) (uuid? (:lease-id a))))))
         (case (:halt-reason page)
           :observed-end (nil? (:next-after page))
           :page-budget (and (= 1 (count rows)) (= id (:next-after page)))
           false))))

(defn- reported-lease [page]
  (let [rows (:results page) a (first rows)]
    (when (and (vector? rows) (= 1 (count rows))
               (#{:acquired :reclaimed} (:status a))
               (uuid? (:intent-id a)) (uuid? (:lease-id a)) (text? (:worker-id a)))
      (assoc (select-keys a [:intent-id :lease-id :worker-id])
             :acquisition-status (:status a) :evidence :unverified-worker-page))))

(defn- raw-delivery [a d results cursor]
  (let [lease (assoc (select-keys a [:intent-id :lease-id :worker-id])
                     :acquisition-status (:status a))
        ack (:acknowledgment d) source (:source-result d) emissions (:results source)
        correlated? (and (map? d) (uuid? (:receipt-id d))
                         (= (select-keys a [:intent-id :lease-id])
                            (select-keys d [:intent-id :lease-id])))
        receipt (when correlated? {:receipt-id (:receipt-id d)
                                   :receipt-evidence :unverified-post-dispatch-receipt})
        details {:delivery-status (:status d) :acknowledgment-status (:status ack)}
        accepted? (= :dispatch-accepted (:status d))
        negative? (= :dispatched-unacknowledged (:status d))]
    (cond
      (not accepted?)
      {:failure (interrupted results cursor
                             (merge lease (when (and negative? (#{:contended :unavailable} (:status ack))) receipt))
                             :acknowledgment :not-dispatch-accepted
                             (assoc details :source-acceptance (if negative? :reported :unknown)))}
      (and correlated? (some? (:progression a)) (= (:progression a) (:progression d))
           (map? ack) (= :acknowledged (:status ack))
           (= (select-keys d [:intent-id :lease-id :receipt-id])
              (select-keys ack [:intent-id :lease-id :receipt-id]))
           (map? source) (= :emitted-many (:status source))
           (vector? emissions) (seq emissions) (integer? (:count source))
           (= (count emissions) (:count source))
           (every? #(#{:emitted :queued} (:status %)) emissions))
      {:result (assoc lease :status :dispatch-accepted :receipt-id (:receipt-id d)
                           :progression (:progression d))}
      :else
      {:failure (interrupted results cursor (merge lease receipt) :delivery :invalid-delivery-result
                             (assoc details :source-acceptance :reported))})))

(defn- raw-attempt [step opts results cursor]
  (let [page (:page step) a (first (:results page))]
    (cond
      (not (page-valid? page cursor (:worker-id opts)))
      {:failure (interrupted results cursor (reported-lease page) :acquisition :acquisition-exception
                             {:original-error/type :gesso.live.outbox/invalid-cycle})}
      (empty? (:results page)) {:end? true}
      (#{:contended :unavailable} (:status a))
      {:result {:intent-id (:intent-id a) :acquisition-status (:status a) :reason (:reason a)}}
      (#{:acquired :reclaimed} (:status a))
      (when (and (contains? step :delivery-report)
                 (or (nil? (:delivery-report step)) (map? (:delivery-report step))))
        (assoc (raw-delivery a (:delivery-report step) results cursor) :dispatch a))
      :else
      {:failure (interrupted results cursor
                             {:intent-id (:intent-id a) :lease-id (:lease-id a)
                              :worker-id (:worker-id a) :acquisition-status (:status a)}
                             :acquisition :unexpected-acquisition-status {:status (:status a)})})))

(defn predict-cycle
  "Predict a bounded cycle over selected declarative adapter reports. Each
   reached step is an empty page, an acquisition exception without a known
   lease, a single well-formed candidate with optional delivery outcome,
   or a selected raw returned page and delivery report.
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
          (and (map? step) (= :raw-page (:kind step))
               (#{#{:kind :page} #{:kind :page :delivery-report}} (set (keys step)))
               (raw-page? (:page step)))
          (let [attempt (raw-attempt step opts results cursor)
                calls (cond-> calls (:dispatch attempt) (conj [:dispatch (:dispatch attempt)]))]
            (cond
              (nil? attempt) unsupported
              (:failure attempt) (assoc base :status :predicted :calls calls :failure (:failure attempt))
              (:end? attempt) (complete results false nil)
              :else (let [done (conj results (:result attempt))
                          more? (= :page-budget (get-in step [:page :halt-reason]))
                          id (get-in step [:page :next-after])]
                      (if (and more? (> remaining 1))
                        (recur id (dec remaining) done calls (next todo))
                        (assoc base :status :predicted :calls calls :result (summary opts done more? id))))))
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
