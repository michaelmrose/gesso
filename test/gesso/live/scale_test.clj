(ns gesso.live.scale-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.bus :as bus]
   [gesso.live.transport.sse :as sse])
  (:import
   [java.io Writer]
   [java.util.concurrent LinkedBlockingQueue TimeUnit]
   [java.util.concurrent.atomic AtomicLong LongAdder]))

;; -----------------------------------------------------------------------------
;; Scope of this test
;; -----------------------------------------------------------------------------
;;
;; This scale harness exercises the current in-process live hot path:
;;
;;   bus/publish!
;;     -> matching-subscriber-ids
;;     -> notify-subscribers!
;;     -> subscriber :send!
;;     -> sse/event->sse
;;     -> LinkedBlockingQueue.offer
;;
;; Each simulated client owns a queue. The moment an event frame is offered to
;; that queue is treated as the server-side "client receive" point.
;;
;; This does NOT test:
;; - real browser EventSource connections
;; - Jetty/Ring streaming thread behavior
;; - socket/kernel buffering
;; - HTMX DOM swap latency
;; - fragment GET latency
;; - gesso.live.oob pending-fragment behavior
;;
;; It DOES test:
;; - subscription indexing
;; - matching
;; - fanout
;; - serialized notify behavior
;; - SSE frame generation
;; - per-client queue delivery
;; - correctness of every delivered event under increasingly large fanout

;; -----------------------------------------------------------------------------
;; Parameterized load tiers
;; -----------------------------------------------------------------------------

(def tiers
  [{:tier 1
    :label "tiny"
    :subscribers 10
    :events 50}

   {:tier 2
    :label "small"
    :subscribers 100
    :events 100}

   {:tier 3
    :label "medium"
    :subscribers 500
    :events 100}

   {:tier 4
    :label "large"
    :subscribers 1000
    :events 100}

   {:tier 5
    :label "very-large"
    :subscribers 2000
    :events 100}

   {:tier 6
    :label "huge"
    :subscribers 10000
    :events 100}

   {:tier 7
    :label "absurd"
    :subscribers 100000
    :events 100}])

(def default-max-tier
  7)

(def max-tier-property
  "gesso.live.scale.max-tier")

(def max-tier-env
  "GESSO_LIVE_SCALE_MAX_TIER")

(defn parse-int
  [s]
  (when s
    (try
      (Integer/parseInt (str s))
      (catch NumberFormatException _
        nil))))

(defn runtime-max-tier
  []
  (or (parse-int (System/getProperty max-tier-property))
      (parse-int (System/getenv max-tier-env))
      default-max-tier))

(defn selected-tiers
  []
  (let [max-tier (runtime-max-tier)]
    (filterv #(<= (:tier %) max-tier) tiers)))

;; -----------------------------------------------------------------------------
;; Performance standards
;; -----------------------------------------------------------------------------
;;
;; A tier is first correctness-gated.
;;
;; If any client has any incorrect result of any kind, the classification is
;; :failed regardless of latency.
;;
;; Only a perfectly correct run is classified by latency.
;;
;; avg-receive-ms:
;;   average latency across all client/event deliveries
;;
;; worst-receive-ms:
;;   maximum latency across all client/event deliveries

(def performance-standards
  [{:standard :best
    :avg-receive-ms 25.0
    :worst-receive-ms 250.0}

   {:standard :better
    :avg-receive-ms 75.0
    :worst-receive-ms 750.0}

   {:standard :good
    :avg-receive-ms 200.0
    :worst-receive-ms 2000.0}])

(def required-standard
  ;; Anything below :best is a failing test, but the report still includes the
  ;; actual classification and measurements.
  :best)

(def suppress-debug-output?
  ;; Current bus code has prn calls in the hot path. If this is false, the scale
  ;; test mostly benchmarks stdout instead of the live system.
  true)

;; -----------------------------------------------------------------------------
;; Live matching setup
;; -----------------------------------------------------------------------------

(def shared-entry
  [:demo-counter "global-shared-counter"])

(def changed
  {:entity/type :demo-counter
   :entity/id "global-shared-counter"
   :change/kind :updated})

(def matcher
  {:subscription->entries
   (fn [subscription]
     (if subscription
       [subscription]
       []))

   :changed->entries
   (fn [_ctx changed]
     (let [entity-type (:entity/type changed)
           entity-id (:entity/id changed)]
       (if (and entity-type entity-id)
         [[entity-type entity-id]]
         [])))})

;; -----------------------------------------------------------------------------
;; Output suppression
;; -----------------------------------------------------------------------------

(def null-writer
  (proxy [Writer] []
    (write
      ([x]
       nil)
      ([x off len]
       nil))
    (flush []
      nil)
    (close []
      nil)))

(defmacro with-output-suppressed
  [& body]
  `(if suppress-debug-output?
     (binding [*out* null-writer
               *err* null-writer]
       ~@body)
     (do
       ~@body)))

;; -----------------------------------------------------------------------------
;; Latency stats
;; -----------------------------------------------------------------------------

(defn ns->ms
  [n]
  (/ (double n) 1000000.0))

(defn make-latency-stats
  []
  {:count (LongAdder.)
   :total-ns (LongAdder.)
   :worst-ns (AtomicLong. 0)})

(defn update-max!
  [^AtomicLong a n]
  (loop []
    (let [old (.get a)]
      (when (> n old)
        (when-not (.compareAndSet a old n)
          (recur))))))

(defn record-latency!
  [{:keys [^LongAdder count ^LongAdder total-ns ^AtomicLong worst-ns]} latency-ns]
  (.increment count)
  (.add total-ns latency-ns)
  (update-max! worst-ns latency-ns)
  nil)

(defn latency-summary
  [{:keys [^LongAdder count ^LongAdder total-ns ^AtomicLong worst-ns]}]
  (let [n (.sum count)
        total (.sum total-ns)
        avg-ns (if (pos? n)
                 (/ (double total) (double n))
                 0.0)
        worst (.get worst-ns)]
    {:delivery-count n
     :avg-receive-ms (ns->ms avg-ns)
     :worst-receive-ms (ns->ms worst)}))

(defn classify-latency
  [{:keys [avg-receive-ms worst-receive-ms]}]
  (or
   (some
    (fn [{:keys [standard]
          avg-limit :avg-receive-ms
          worst-limit :worst-receive-ms}]
      (when (and (<= avg-receive-ms avg-limit)
                 (<= worst-receive-ms worst-limit))
        standard))
    performance-standards)
   :none))

(def standard-rank
  {:failed 0
   :none 0
   :good 1
   :better 2
   :best 3})

(defn meets-required-standard?
  [classification]
  (>= (standard-rank classification)
      (standard-rank required-standard)))

;; -----------------------------------------------------------------------------
;; SSE frame parsing
;; -----------------------------------------------------------------------------

(defn frame-seq
  [frame]
  (when-let [[_ n] (re-find #":seq\s+(\d+)" (str frame))]
    (Long/parseLong n)))

(defn frame-sent-ns
  [frame]
  (when-let [[_ n] (re-find #":sent-ns\s+(\d+)" (str frame))]
    (Long/parseLong n)))

;; -----------------------------------------------------------------------------
;; Simulated clients
;; -----------------------------------------------------------------------------

(defn recording-queue
  "Return a queue that records receive latency when the live system offers an SSE
   frame into it."
  [stats]
  (proxy [LinkedBlockingQueue] []
    (offer
      ([frame]
       (let [sent-ns (frame-sent-ns frame)
             now-ns (System/nanoTime)]
         (when sent-ns
           (record-latency! stats (- now-ns sent-ns))))
       (proxy-super offer frame))
      ([frame timeout unit]
       (let [sent-ns (frame-sent-ns frame)
             now-ns (System/nanoTime)]
         (when sent-ns
           (record-latency! stats (- now-ns sent-ns))))
       (proxy-super offer frame timeout unit)))))

(defn make-client
  [i stats]
  (let [queue (recording-queue stats)
        closed? (atom false)
        subscriber (sse/build-subscriber
                    {:subscription shared-entry
                     :queue queue
                     :closed? closed?
                     :meta {:transport :scale-test
                            :client/id i}})]
    {:client/id i
     :queue queue
     :closed? closed?
     :subscriber subscriber}))

(defn subscribe-clients!
  [live-bus subscriber-count stats]
  (let [clients (mapv #(make-client % stats)
                      (range subscriber-count))]
    (doseq [{:keys [subscriber]} clients]
      (bus/subscribe! live-bus subscriber))
    clients))

(defn unsubscribe-clients!
  [live-bus clients]
  (doseq [{:keys [subscriber closed?]} clients]
    (reset! closed? true)
    (bus/unsubscribe! live-bus (:subscriber/id subscriber))))

(defn poll-frame-now
  [^LinkedBlockingQueue q]
  (.poll q 0 TimeUnit/MILLISECONDS))

;; -----------------------------------------------------------------------------
;; Publishing
;; -----------------------------------------------------------------------------

(defn event
  [i]
  {:event "live-update"
   :changed changed
   :consistency-token nil
   :data {:seq i
          :sent-ns (System/nanoTime)}})

;; -----------------------------------------------------------------------------
;; Correctness tracking
;; -----------------------------------------------------------------------------

(defn make-correctness-state
  [subscribers events]
  {:ok? true
   :client-count subscribers
   :event-count events
   :total-expected-deliveries (* subscribers events)
   :failed-client-count 0
   :missing-count 0
   :unparseable-count 0
   :wrong-order-count 0
   :extra-count 0
   :failures []})

(def max-recorded-failures
  10)

(defn add-failure
  [state failure]
  (-> state
      (assoc :ok? false)
      (update :failed-client-count inc)
      (update :failures
              (fn [failures]
                (if (< (count failures) max-recorded-failures)
                  (conj failures failure)
                  failures)))))

(defn note-missing
  [state client-id expected-seq]
  (-> state
      (update :missing-count inc)
      (add-failure {:client/id client-id
                    :kind :missing
                    :expected-seq expected-seq})))

(defn note-unparseable
  [state client-id expected-seq frame]
  (let [s (str frame)]
    (-> state
        (update :unparseable-count inc)
        (add-failure {:client/id client-id
                      :kind :unparseable
                      :expected-seq expected-seq
                      :frame-preview (subs s 0 (min 120 (count s)))}))))

(defn note-wrong-order
  [state client-id expected-seq actual-seq]
  (-> state
      (update :wrong-order-count inc)
      (add-failure {:client/id client-id
                    :kind :wrong-order
                    :expected-seq expected-seq
                    :actual-seq actual-seq})))

(defn note-extra
  [state client-id frame]
  (let [s (str frame)]
    (-> state
        (update :extra-count inc)
        (add-failure {:client/id client-id
                      :kind :extra
                      :frame-preview (subs s 0 (min 120 (count s)))}))))

(defn check-client-frame
  [state {:keys [client/id queue]} expected-seq]
  (let [frame (poll-frame-now queue)]
    (cond
      (nil? frame)
      (note-missing state id expected-seq)

      (nil? (frame-seq frame))
      (note-unparseable state id expected-seq frame)

      (not= expected-seq (frame-seq frame))
      (note-wrong-order state id expected-seq (frame-seq frame))

      :else
      state)))

(defn check-extra-events
  [state clients]
  (reduce
   (fn [state {:keys [client/id queue]}]
     (if-let [frame (poll-frame-now queue)]
       (note-extra state id frame)
       state))
   state
   clients))

(defn publish-and-check-event!
  [live-bus clients correctness expected-seq]
  (with-output-suppressed
    (bus/publish! live-bus (event expected-seq)))
  (reduce
   (fn [state client]
     (check-client-frame state client expected-seq))
   correctness
   clients))

(defn publish-and-check-all!
  [live-bus clients events subscribers]
  (let [initial (make-correctness-state subscribers events)
        checked (loop [i 0
                       state initial]
                  (if (= i events)
                    state
                    (recur (inc i)
                           (publish-and-check-event!
                            live-bus
                            clients
                            state
                            i))))]
    (check-extra-events checked clients)))

(defn classify-run
  [{:keys [correctness latency expected-deliveries subscribed?]}]
  (cond
    (not subscribed?)
    :failed

    (not (:ok? correctness))
    :failed

    (not= expected-deliveries (:delivery-count latency))
    :failed

    :else
    (classify-latency latency)))

;; -----------------------------------------------------------------------------
;; Reporting
;; -----------------------------------------------------------------------------

(defn standard-by-name
  [standard]
  (some #(when (= standard (:standard %)) %)
        performance-standards))

(defn fmt-ms
  [x]
  (format "%.3f" (double x)))

(defn over-by
  [actual limit]
  (max 0.0 (- (double actual) (double limit))))

(defn correctness-status
  [correctness]
  (if (:ok? correctness)
    "yes"
    "NO"))

(defn result-status
  [classification]
  (if (meets-required-standard? classification)
    "PASS"
    "FAIL"))

(defn failure-reason
  [{:keys [classification correctness latency]}]
  (let [required (standard-by-name required-standard)
        good (standard-by-name :good)
        avg-limit (:avg-receive-ms required)
        worst-limit (:worst-receive-ms required)
        avg (:avg-receive-ms latency)
        worst (:worst-receive-ms latency)]
    (cond
      (not (:ok? correctness))
      (str "incorrect deliveries"
           " missing=" (:missing-count correctness)
           " unparseable=" (:unparseable-count correctness)
           " wrong-order=" (:wrong-order-count correctness)
           " extra=" (:extra-count correctness))

      (= classification :failed)
      "failed correctness or delivery-count gate"

      (= classification :none)
      (str "missed good"
           ": avg over by "
           (fmt-ms (over-by avg (:avg-receive-ms good)))
           "ms, worst over by "
           (fmt-ms (over-by worst (:worst-receive-ms good)))
           "ms")

      (not (meets-required-standard? classification))
      (str "missed " (name required-standard)
           ": avg over by " (fmt-ms (over-by avg avg-limit)) "ms"
           ", worst over by " (fmt-ms (over-by worst worst-limit)) "ms")

      :else
      "-")))

(defn report-row
  [{:keys [tier label subscribers events expected-deliveries
           subscribe-count correctness latency classification] :as report}]
  (let [required (standard-by-name required-standard)]
    (format
     "%4d  %-11s %8d %8d %7d %11d  %-7s %-7s %-8s %9s %10s %10s %11s  %s"
     tier
     label
     subscribers
     subscribe-count
     events
     expected-deliveries
     (correctness-status correctness)
     (name classification)
     (result-status classification)
     (fmt-ms (:avg-receive-ms latency))
     (fmt-ms (:avg-receive-ms required))
     (fmt-ms (:worst-receive-ms latency))
     (fmt-ms (:worst-receive-ms required))
     (failure-reason report))))

(defn print-report!
  [reports]
  (println)
  (println "Gesso live simulated client-queue receive-latency scale test")
  (println "============================================================")
  (println "Correctness gate: any missing, extra, unordered, or unparseable delivery => failed")
  (println "Required performance standard:" required-standard)
  (println "Runtime max tier:" (runtime-max-tier))
  (println "Set max tier with JVM property:" (str "-D" max-tier-property "=6"))
  (println "Or env var:" (str max-tier-env "=6"))
  (println)
  (println "Standards:")
  (doseq [{:keys [standard avg-receive-ms worst-receive-ms]} performance-standards]
    (println
     (format "  %-6s avg <= %8s ms, worst <= %8s ms"
             (name standard)
             (fmt-ms avg-receive-ms)
             (fmt-ms worst-receive-ms))))
  (println)
  (println
   (format
    "%4s  %-11s %8s %8s %7s %11s  %-7s %-7s %-8s %9s %10s %10s %11s  %s"
    "Tier" "Label" "Users" "Subbed" "Events" "Deliveries"
    "Correct" "Class" "Result"
    "Avg" "AvgLimit" "Worst" "WorstLimit" "Reason"))
  (println
   (apply str (repeat 156 "-")))
  (doseq [report reports]
    (println (report-row report)))
  (println))

;; -----------------------------------------------------------------------------
;; Runner
;; -----------------------------------------------------------------------------

(defn run-tier!
  [{:keys [subscribers events] :as tier}]
  (let [live-bus (bus/memory-bus matcher)
        stats (make-latency-stats)
        clients (with-output-suppressed
                  (subscribe-clients! live-bus subscribers stats))
        subscribe-count (count (bus/subscribers-snapshot live-bus))
        expected-deliveries (* subscribers events)]
    (try
      (let [correctness (publish-and-check-all!
                         live-bus
                         clients
                         events
                         subscribers)
            latency (latency-summary stats)
            classification (classify-run
                            {:subscribed? (= subscribers subscribe-count)
                             :correctness correctness
                             :latency latency
                             :expected-deliveries expected-deliveries})]
        (merge tier
               {:expected-deliveries expected-deliveries
                :subscribe-count subscribe-count
                :correctness correctness
                :latency latency
                :classification classification}))
      (finally
        (with-output-suppressed
          (unsubscribe-clients! live-bus clients))))))

(defn run-scale!
  ([] (run-scale! (selected-tiers)))
  ([tiers]
   (mapv run-tier! tiers)))

;; -----------------------------------------------------------------------------
;; Test
;; -----------------------------------------------------------------------------

(deftest receive-latency-scale-test
  (testing "current live bus + SSE queue bridge receive latency across scaled tiers"
    (let [reports (run-scale!)]
      (print-report! reports)

      (doseq [{:keys [tier label subscribers subscribe-count correctness
                      classification latency] :as report}
              reports]
        (testing (str "tier " tier " / " label)
          (is (= subscribers subscribe-count)
              (str "Not all simulated clients were subscribed: "
                   (pr-str {:subscribers subscribers
                            :subscribe-count subscribe-count
                            :report (report-row report)})))

          (is (:ok? correctness)
              (str "Correctness failure. Any incorrect result fails the tier: "
                   (pr-str {:classification classification
                            :correctness correctness
                            :latency latency
                            :report (report-row report)})))

          (is (meets-required-standard? classification)
              (str "Tier did not reach required standard "
                   required-standard
                   ". Measurements and actual classification: "
                   (pr-str {:classification classification
                            :correctness correctness
                            :latency latency
                            :report (report-row report)}))))))))
