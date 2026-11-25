(ns gesso.live.outbox.history-controller
  "Owner-driven lifecycle for bounded mapped history observations. The owner
   supplies trusted load/save callbacks and a deployment-specific secret.
   No scheduler, request-path authority, or certification is introduced."
  (:require [clojure.string :as str]
            [gesso.live.outbox.history-coverage :as history])
  (:import [java.util.concurrent TimeUnit]
           [java.util.concurrent.locks ReentrantLock]))

(defn- reject! [reason]
  (throw (ex-info "Mapped history controller not available." {:reason reason})))

(defn validate-options! [opts]
  (when-not (and (map? opts)
                 (= #{:key :deployment-id :load! :save! :max-steps} (set (keys opts)))
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
   A failed advance retains the last successfully persisted cursor."
  [node admission opts]
  (let [{:keys [key deployment-id load! save! max-steps]} (validate-options! opts)
        key (aclone ^bytes key)
        sealed (load!)
        cursor (atom (when (some? sealed)
                       (history/restore-checkpoint! node admission sealed key deployment-id)))
        state (atom {:phase :open :report nil :last-error nil})
        lock (ReentrantLock.)
        advance! (fn []
                   (when (or (.isHeldByCurrentThread lock) (not (.tryLock lock)))
                     (reject! :history-controller-busy))
                   (try
                     (when-not (= :open (:phase @state))
                       (reject! :history-controller-closed))
                     (try
                       (let [result (history/audit-chunk! node admission max-steps @cursor)
                             next-cursor (:checkpoint result)
                             report (dissoc result :checkpoint)]
                         (save! (history/seal-checkpoint! node admission next-cursor key deployment-id))
                         (reset! cursor next-cursor)
                         (swap! state assoc :report report :last-error nil)
                         report)
                       (catch Exception e
                         (when (instance? InterruptedException e)
                           (.interrupt (Thread/currentThread)))
                         ;; Do not retain callback exceptions, secrets or raw values.
                         (swap! state assoc :last-error :history-advance-failed)
                         (throw e)))
                     (finally (.unlock lock))))
        close! (fn [timeout-ms]
                 (when (.isHeldByCurrentThread lock)
                   (reject! :history-controller-busy))
                 (swap! state #(if (= :closed (:phase %)) % (assoc % :phase :closing)))
                 (let [acquired? (try (.tryLock lock (long timeout-ms) TimeUnit/MILLISECONDS)
                                      (catch InterruptedException e
                                        (.interrupt (Thread/currentThread))
                                        (throw e)))]
                   (when-not acquired? (reject! :history-controller-not-drained))
                   (try
                     (swap! state assoc :phase :closed)
                     (finally (.unlock lock)))
                   nil))]
    (advance!)
    {:advance! advance! :status (fn [] @state) :close! close!}))
