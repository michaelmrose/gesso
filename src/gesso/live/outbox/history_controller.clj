(ns gesso.live.outbox.history-controller
  "Owner-driven lifecycle for bounded mapped history observations. The owner
   supplies trusted load/save callbacks and a deployment-specific secret.
   Optional paced scheduling shares the owner-driven admission fence.
   No request-path authority or certification is introduced."
  (:require [clojure.string :as str]
            [gesso.live.outbox.history-coverage :as history]
            [gesso.live.outbox.history-scheduler :as scheduler]
            [gesso.model.inductive-plan :as inductive])
  (:import [java.time Instant]
           [java.util.concurrent TimeUnit]
           [java.util.concurrent.locks ReentrantLock]))

(defn- reject! [reason]
  (throw (ex-info "Mapped history controller not available." {:reason reason})))

(defn validate-options! [opts]
  (when-not (and (map? opts)
                 (= #{:key :deployment-id :load! :save! :max-steps} (disj (set (keys opts)) :schedule))
                 (or (not (contains? opts :schedule)) (scheduler/valid-options? (:schedule opts)))
                 (bytes? (:key opts)) (= 32 (alength ^bytes (:key opts)))
                 (string? (:deployment-id opts)) (not (str/blank? (:deployment-id opts)))
                 (<= (count (:deployment-id opts)) 512)
                 (fn? (:load! opts)) (fn? (:save! opts))
                 (integer? (:max-steps opts)) (<= 1 (:max-steps opts) 128))
    (reject! :invalid-history-controller-options))
  opts)

(defn start!
  "Restore an optional sealed checkpoint, verify and persist one bounded
   chunk, then expose owner-only controls. load! returns nil or sealed EDN;
   save! must durably replace it or throw. The owner must ensure exclusive
   storage ownership and atomic persistence. Startup errors propagate;
   unresolved observed steps are diagnostic and do not prevent startup.
   A failed advance retains the last successfully persisted cursor.
   Optional :schedule runs one paced chunk at a time after startup."
  [node admission opts]
  (let [{:keys [key deployment-id load! save! max-steps]} (validate-options! opts)
        key (aclone ^bytes key)
        sealed (load!)
        cursor (atom (when (some? sealed)
                       (history/restore-checkpoint! node admission sealed key deployment-id)))
        state (atom {:phase :open :report nil :last-error nil
                     :source (select-keys (inductive/describe-mapped-admission admission)
                                          [:catalog-fingerprint :induction-fingerprint])
                     :advance-in-progress? false :last-success-at nil :last-failure-at nil})
        lock (ReentrantLock.)
        scheduled (atom nil)
        advance! (fn []
                   (when (or (.isHeldByCurrentThread lock) (not (.tryLock lock)))
                     (reject! :history-controller-busy))
                   (try
                     (when-not (= :open (:phase @state))
                       (reject! :history-controller-closed))
                     (swap! state assoc :advance-in-progress? true)
                     (try
                       (let [result (history/audit-chunk! node admission max-steps @cursor)
                             next-cursor (:checkpoint result)
                             report (dissoc result :checkpoint)]
                         (save! (history/seal-checkpoint! node admission next-cursor key deployment-id))
                         (reset! cursor next-cursor)
                         (swap! state assoc :report report :last-error nil
                                :last-success-at (str (Instant/now)))
                         report)
                       (catch Exception e
                         (when (instance? InterruptedException e)
                           (.interrupt (Thread/currentThread)))
                         ;; Do not retain callback exceptions, secrets or raw values.
                         (swap! state assoc :last-error :history-advance-failed
                                :last-failure-at (str (Instant/now)))
                         (throw e))
                       (finally (swap! state assoc :advance-in-progress? false)))
                     (finally (.unlock lock))))
        close! (fn [timeout-ms]
                 (when (.isHeldByCurrentThread lock)
                   (reject! :history-controller-busy))
                 (swap! state #(if (= :closed (:phase %)) % (assoc % :phase :closing)))
                 (let [deadline (+ (System/nanoTime) (.toNanos TimeUnit/MILLISECONDS (long timeout-ms)))
                       remaining #(max 0 (- deadline (System/nanoTime)))
                       _ (when-let [s @scheduled]
                           (when-not ((:stop! s) (remaining))
                             (reject! :history-controller-not-drained)))
                       acquired? (try (.tryLock lock (long (remaining)) TimeUnit/NANOSECONDS)
                                      (catch InterruptedException e
                                        (.interrupt (Thread/currentThread))
                                        (throw e)))]
                   (when-not acquired? (reject! :history-controller-not-drained))
                   (try
                     (swap! state assoc :phase :closed)
                     (finally (.unlock lock)))
                   nil))]
    (let [initial (advance!)]
      (when-let [schedule (:schedule opts)]
        (reset! scheduled (scheduler/start! schedule advance! #(= :open (:phase @state)) initial))))
    {:advance! advance!
     :status (fn [] (cond-> @state @scheduled (assoc :schedule ((:status @scheduled)))))
     :close! close!}))
