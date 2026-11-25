(ns gesso.assurance.reference-clock
  "Independent pinned retained transaction clock decisions."
  (:import (java.time Instant)))

(defn- frame-instant [micros]
  (when (and (integer? micros) (<= Long/MIN_VALUE micros Long/MAX_VALUE))
    (Instant/ofEpochSecond (Math/floorDiv (long micros) 1000000)
                           (* 1000 (Math/floorMod (long micros) 1000000)))))

(defn implicit-system-time [micros prior]
  ;; The pinned Indexer uses max(log timestamp, latest completed time +
  ;; one microsecond) for implicit times, including after aborted txs.
  (when-let [stamp (frame-instant micros)]
    (cond
      (nil? prior) stamp
      (not (and (instance? Instant prior)
                (zero? (mod (.getNano ^Instant prior) 1000))
                (not (.isBefore ^Instant prior (frame-instant Long/MIN_VALUE)))
                (.isBefore ^Instant prior (frame-instant Long/MAX_VALUE)))) nil
      (.isAfter stamp ^Instant prior) stamp
      :else (.plusNanos ^Instant prior 1000))))

(def clock-profile :xtdb-2.2.0-beta1/selected-retained-clock-decision-v1)

(defn- clock-instant? [x]
  (and (instance? Instant x)
       (zero? (mod (.getNano ^Instant x) 1000))
       (not (.isBefore ^Instant x (frame-instant Long/MIN_VALUE)))
       (not (.isAfter ^Instant x (frame-instant Long/MAX_VALUE)))))

(defn resolve-retained-clock
  "Predict the pinned indexer's clock decision from independently acquired
   frame microseconds, requested envelope time (nil for implicit) and the
   preceding completed transaction time (nil at genesis). Equal explicit
   times are admitted; older requests are rejected before operations run
   and receive the default clock time. Admission here is clock-only, not a
   prediction of commit. No provider results or authenticated provenance."
  [frame-micros requested prior]
  (let [report {:profile clock-profile :authority :none :certificate :not-issued}
        fail #(assoc report :status :unsupported :reason %)]
    (cond
      (nil? (frame-instant frame-micros)) (fail :invalid-clock-frame)
      (not (or (nil? requested) (clock-instant? requested))) (fail :invalid-requested-system-time)
      (not (or (nil? prior) (clock-instant? prior))) (fail :invalid-prior-system-time)
      :else
      (let [older? (and requested prior (.isBefore ^Instant requested ^Instant prior))
            default? (or (nil? requested) older?)
            resolved (if default? (implicit-system-time frame-micros prior) requested)]
        (if-not resolved
          (fail :retained-clock-overflow)
          (assoc report :status :resolved :system-time resolved
                 :decision (if older? :reject-before-execution :execute)
                 :time-source (cond (not default?) :explicit
                                    (= resolved (frame-instant frame-micros)) :frame
                                    :else :smoothed)
                 :reason (if older? :invalid-system-time :clock-admitted)))))))

(def clocked-input-profile :selected-retained-clocked-replay-input-v1)

