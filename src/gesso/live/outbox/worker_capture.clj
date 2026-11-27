(ns gesso.live.outbox.worker-capture
  "Internal installed-host worker capture ownership boundary."
  (:require [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.lifecycle :as lifecycle]))

(defn- reject! [reason data]
  (throw (ex-info "Gesso single-node outbox host not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-host
                         :reason reason))))

(def ^:private worker-capture-hosts (java.util.WeakHashMap.))
(def ^:private worker-capture-origins (java.util.WeakHashMap.))
(def ^:private original-outcome-capture xtdb/capture-policy-outcomes!)
(def ^:private original-outcome-match xtdb/policy-capture-origin-matches?)
(def ^:private original-policy-current xtdb/submission-policy-current?)
(def ^:private original-lifecycle-reference lifecycle/lifecycle-reference)

(defn- worker-capture-boundaries-current? []
  (and (identical? original-outcome-capture xtdb/capture-policy-outcomes!)
       (identical? original-outcome-match xtdb/policy-capture-origin-matches?)
       (identical? original-policy-current xtdb/submission-policy-current?)
       (identical? original-lifecycle-reference lifecycle/lifecycle-reference)))

(defn- register-worker-capture-host! [system token active]
  (let [worker (get system managed/handle-key)]
    (locking worker-capture-hosts
      (.put worker-capture-hosts token
            {:system (java.lang.ref.WeakReference. system)
             :active active :policy-token (:gesso.live.outbox.host/mapped-policy-token system)
             :lifecycle-ref (original-lifecycle-reference worker)})))
  system)

(defn- worker-capture-owner [system]
  (let [entry (locking worker-capture-hosts
                (.get worker-capture-hosts (:gesso.live.outbox.host/worker-capture-token system)))]
    (when (and entry (identical? system (.get ^java.lang.ref.WeakReference (:system entry))))
      entry)))

(defn- owned-worker-call? [system entry call]
  (let [origin (get-in call [:admission :worker-origin])]
    (and (= :worker (get-in call [:admission :kind]))
         (= :matched-scheduled-maintenance (:call-chain origin))
         (= :scheduled-local-cycle (:origin origin))
         (= :not-established (:authentication origin))
         (= (:lifecycle-ref entry) (:lifecycle-ref origin))
         (= (get system managed/source-id-key) (:source-id origin))
         (= (get-in system [managed/options-key :worker-id]) (:worker-id origin)))))

(defn capture-worker-outcomes!
  "Observe a bounded window on the exact installed mapped host. All captured
   calls must carry the owning lifecycle's matched maintenance origin.
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
               (every? #(owned-worker-call? system entry %) (:calls report)))
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
