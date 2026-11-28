(ns gesso.live.outbox.worker-capture
  "Internal installed-host worker capture ownership boundary."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.core :as live]
            [gesso.live.source :as source])
  (:import [xtdb.api TransactionKey]))

(defn- reject! [reason data]
  (throw (ex-info "Gesso single-node outbox host not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-host
                         :reason reason))))

(def ^:private worker-capture-hosts (java.util.WeakHashMap.))
(def ^:private worker-capture-origins (java.util.WeakHashMap.))
(def ^:private original-outcome-capture xtdb/capture-policy-outcomes!)
(def ^:private original-outcome-match xtdb/policy-capture-origin-matches?)
(def ^:private original-policy-current xtdb/submission-policy-current?)
(def ^:private original-lineage-members lifecycle/managed-lineage-members)

(defn- worker-capture-boundaries-current? []
  (and (identical? original-outcome-capture xtdb/capture-policy-outcomes!)
       (identical? original-outcome-match xtdb/policy-capture-origin-matches?)
       (identical? original-policy-current xtdb/submission-policy-current?)
       (identical? original-lineage-members lifecycle/managed-lineage-members)))

(defn- register-worker-capture-host!
  ([system token active lock]
   (register-worker-capture-host! system token active lock nil))
  ([system token active lock authority]
  (let [worker (get system managed/handle-key)]
    (locking worker-capture-hosts
      (.put worker-capture-hosts token
            {:system (java.lang.ref.WeakReference. system)
             :active active :lock lock :owner-ref (str (random-uuid))
             :authority authority
             :policy-token (:gesso.live.outbox.host/mapped-policy-token system)
             :root worker :source-target (get-in worker [:system :source])})))
  system))

(defn- worker-capture-owner [system]
  (let [entry (locking worker-capture-hosts
                (.get worker-capture-hosts (:gesso.live.outbox.host/worker-capture-token system)))]
    (when (and entry (identical? system (.get ^java.lang.ref.WeakReference (:system entry))))
      entry)))

(defn with-worker-authority-owner!
  "Internal owner-locked access to the startup-selected authority declaration.
   The callback must qualify its exact issued cut separately."
  [system f]
  (let [entry (worker-capture-owner system)]
    (when-not entry (reject! :unissued-worker-capture-host {}))
    (locking (:lock entry)
      (letfn [(check! []
                (when-not @(:active entry) (reject! :worker-capture-host-retired {}))
                (when-not (worker-capture-boundaries-current?)
                  (reject! :worker-capture-boundary-replaced {}))
                (when-not (original-policy-current (:xtdb/connectable system) (:policy-token entry))
                  (reject! :worker-capture-policy-mismatch {})))]
        (check!)
        (let [result (f (:authority entry))]
          (check!)
          result)))))

(defn qualify-installed-worker!
  "Match selected historical origins to the exact registered owner's managed
   scheduler lineage. Returns local installation identity, not authentication.
   The caller must independently establish issuance of the selected origins."
  [system origins]
  (let [entry (worker-capture-owner system)
        node (:xtdb/connectable system)]
    (when-not entry (reject! :unissued-worker-capture-host {}))
    (locking (:lock entry)
      (letfn [(read-binding []
                (when-not @(:active entry) (reject! :worker-capture-host-retired {}))
                (when-not (worker-capture-boundaries-current?)
                  (reject! :worker-capture-boundary-replaced {}))
                (when-not (original-policy-current node (:policy-token entry))
                  (reject! :worker-capture-policy-mismatch {}))
                (let [members (original-lineage-members (:root entry))]
                  (when-not (and (seq members)
                                 (identical? (:root entry) (get system managed/handle-key))
                                 (every? #(= (get system managed/source-id-key) (:source-id %)) members))
                    (reject! :worker-authority-installation-mismatch {}))
                  {:owner-ref (:owner-ref entry)
                   :root (first members) :current-generation (peek members)
                   :lineage members}))]
        (let [binding (read-binding)
              members (:lineage binding)
              by-ref (into {} (map (juxt :lifecycle-ref identity)) members)]
          (when-not (and (seq origins)
                         (every? (fn [origin]
                                   (= (get by-ref (:lifecycle-ref origin))
                                      (select-keys origin [:lifecycle-ref :source-id :worker-id]))) origins))
            (reject! :worker-authority-installation-mismatch {}))
          (when-not (= binding (read-binding))
            (reject! :worker-authority-installation-changed {}))
          binding)))))

(defn- owned-worker-calls? [system entry calls]
  (let [members (original-lineage-members (:root entry))
        by-ref (into {} (map-indexed (fn [i m] [(:lifecycle-ref m) [i m]])) members)
        origins (mapv #(get-in % [:admission :worker-origin]) calls)]
    (and (seq members) (seq calls)
         (every? (fn [[call origin]]
                   (let [[_ member] (get by-ref (:lifecycle-ref origin))]
                     (and member (= :worker (get-in call [:admission :kind]))
                          (= :matched-scheduled-maintenance (:call-chain origin))
                          (= :scheduled-local-cycle (:origin origin))
                          (= :not-established (:authentication origin))
                          (= member (select-keys origin [:lifecycle-ref :source-id :worker-id]))
                          (= (get system managed/source-id-key) (:source-id origin))
                          (pos-int? (:cycle-number origin)))))
                 (map vector calls origins))
         ;; Calls may begin anywhere in the issued lineage but may not go
         ;; backwards across successor handoffs or within a lifecycle.
         (every? (fn [[a b]] (not (pos? (compare a b))))
                 (partition 2 1 (map (fn [origin]
                                       [(first (get by-ref (:lifecycle-ref origin)))
                                        (:cycle-number origin)]) origins))))))

(defn capture-worker-outcomes!
  "Observe a bounded window on the exact installed mapped host. All captured
   calls must carry the root or an explicitly issued successor's matched
   maintenance origin, in lineage/cycle order.
   Refuse new windows after retirement; invalidate mixed/unowned or empty
   windows without changing the thunk outcome. This is sensitive historical
   evidence, not worker authentication, a durable receipt or writer closure."
  [system limit thunk]
  (let [entry (worker-capture-owner system)
        node (:xtdb/connectable system)]
    (when-not entry (reject! :unissued-worker-capture-host {}))
    (when-not @(:active entry) (reject! :worker-capture-host-retired {}))
    (when-not (worker-capture-boundaries-current?)
      (reject! :worker-capture-boundary-replaced {}))
    (when-not (original-policy-current node (:policy-token entry))
      (reject! :worker-capture-policy-mismatch {}))
    ;; Do not hold the host stop lock around arbitrary thunk execution.
    (let [report (original-outcome-capture node limit thunk)]
      (if (and @(:active entry) (worker-capture-boundaries-current?)
               (original-policy-current node (:policy-token entry))
               (= :matched-local-policy-outcome-window (:policy-window report))
               (original-outcome-match report node)
               (seq (:calls report))
               (owned-worker-calls? system entry (:calls report)))
        (do (locking worker-capture-origins
              (.put worker-capture-origins (:local-origin report)
                    {:host-token (:gesso.live.outbox.host/worker-capture-token system)
                     :report (java.lang.ref.WeakReference. report)}))
            report)
        (-> report (dissoc :local-origin)
            (assoc :policy-window :invalidated-local-worker-outcome-window :calls []))))))

(defn worker-outcome-capture-matches?
  "Recognize only the exact issued historical capture and original owner.
   Remains a historical match after shutdown, not authority to open a window."
  [system report]
  (boolean
   (when (and (worker-capture-owner system) (worker-capture-boundaries-current?))
     (let [issued (locking worker-capture-origins
                    (.get worker-capture-origins (:local-origin report)))]
       (and issued (identical? (:gesso.live.outbox.host/worker-capture-token system) (:host-token issued))
            (identical? report (.get ^java.lang.ref.WeakReference (:report issued)))
            (original-outcome-match report (:xtdb/connectable system)))))))

(def ^:private original-source-boundaries delivery/source-boundaries-current?)
(def ^:private original-worker-match worker-outcome-capture-matches?)
(def ^:private original-worker-binding qualify-installed-worker!)
(def ^:private original-live-closed live/closed?)
(def ^:private original-source-closed source/closed?)

(defn- matched-source-return? [source-id instance-id call]
  (let [{:keys [local-source-acceptance local-dispatch-receipt]} (:admission call)
        identities [:intent-id :lease-id :worker-id :receipt-id]]
    (and (= :matched-local-delivery-handoff (:evidence local-dispatch-receipt))
         (= :gesso.live/synchronous-source (:adapter local-dispatch-receipt))
         (= :accepted (:result local-dispatch-receipt))
         (= :matched-synchronous-source-return (:evidence local-source-acceptance))
         (= (select-keys local-dispatch-receipt identities)
            (select-keys local-source-acceptance identities))
         (= (get-in call [:admission :worker-origin :worker-id]) (:worker-id local-source-acceptance))
         (= (get-in call [:admission :intent-id]) (:intent-id local-source-acceptance))
         (= source-id (:source-id local-source-acceptance))
         (= instance-id (:source-instance-id local-source-acceptance))
         (pos-int? (:count local-source-acceptance))
         (vector? (:statuses local-source-acceptance))
         (= (:count local-source-acceptance) (count (:statuses local-source-acceptance)))
         (every? #{:emitted :queued} (:statuses local-source-acceptance)))))

(def ^:private original-matched-source-return matched-source-return?)

(defn- maintenance-attribution [call]
  (let [{:keys [admission tx-result]} call]
    (when-not (and (= :execute-tx (:api call)) (= :returned (:outcome call))
                   (instance? TransactionKey tx-result))
      (reject! :source-authority-maintenance-result-mismatch {}))
    (assoc (select-keys admission [:intent-id :worker-origin :local-dispatch-receipt
                                  :local-source-acceptance])
           :maintenance (get-in admission [:worker-origin :maintenance])
           :tx-id (.getTxId ^TransactionKey tx-result)
           :system-time (.getSystemTime ^TransactionKey tx-result))))

(def ^:private original-maintenance-attribution maintenance-attribution)

(defn qualify-installed-source!
  "Match the exact issued capture to the installed synchronous source.
   Local correspondence is not authentication or delivery proof."
  [system report]
  (let [entry (worker-capture-owner system)
        current? #(and (identical? original-source-boundaries delivery/source-boundaries-current?)
                       (identical? original-worker-match worker-outcome-capture-matches?)
                       (identical? original-worker-binding qualify-installed-worker!)
                       (identical? original-live-closed live/closed?)
                       (identical? original-source-closed source/closed?)
                       (identical? original-matched-source-return matched-source-return?)
                       (identical? original-maintenance-attribution maintenance-attribution)
                       (worker-capture-boundaries-current?) (original-source-boundaries))]
    (when-not entry (reject! :unissued-worker-capture-host {}))
    (locking (:lock entry)
      (when-not @(:active entry) (reject! :worker-capture-host-retired {}))
      (when-not (current?) (reject! :source-authority-boundary-replaced {}))
      (when-not (original-worker-match system report) (reject! :unissued-worker-capture {}))
      (let [calls (:calls report)
            origins (mapv #(get-in % [:admission :worker-origin]) calls)
            binding (original-worker-binding system origins)
            installed (:gesso.live/system system)
            target (:source-target entry)
            source-id (get system managed/source-id-key)
            instance-id (:id target)
            target-current? #(and (some? target) (some? instance-id)
                                  (identical? installed (get-in entry [:root :system]))
                                  (identical? target (:source installed))
                                  (not (original-live-closed installed))
                                  (not (original-source-closed target)))
            ack? #(= :dispatch-acknowledgment (get-in % [:admission :worker-origin :maintenance]))
            acknowledgments (filterv ack? calls)]
        (when-not (target-current?) (reject! :source-authority-installation-mismatch {}))
        (when-not (and (seq acknowledgments)
                       (every? #(original-matched-source-return source-id instance-id %) acknowledgments)
                       (every? #(not (or (contains? (:admission %) :local-source-acceptance)
                                         (contains? (:admission %) :local-dispatch-receipt))) (remove ack? calls)))
          (reject! :source-authority-observation-mismatch {}))
        (let [attribution (mapv original-maintenance-attribution calls)]
          (when-not (apply < (map :tx-id attribution))
            (reject! :source-authority-maintenance-order-mismatch {}))
          (when-not (and @(:active entry) (current?) (target-current?)
                         (original-worker-match system report)
                         (= binding (original-worker-binding system origins)))
            (reject! :source-authority-installation-changed {}))
          {:owner-ref (:owner-ref binding) :source-id source-id :source-instance-id instance-id
           :dispatch-target :gesso.live/synchronous-source
           :correspondence :matched-installed-synchronous-source
           :maintenance-attribution attribution
           :acknowledgments (mapv #(get-in % [:admission :local-source-acceptance]) acknowledgments)
           :durable-delivery :not-established :browser-receipt :not-established})))))
