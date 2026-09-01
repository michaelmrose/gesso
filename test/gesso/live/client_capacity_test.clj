(ns gesso.live.client-capacity-test
  "Capacity regressions for Gesso Live's ephemeral pending-OOB mailbox.

   Pending OOB delivery is deliberately non-authoritative and at-most-once. It
   therefore must not become an unbounded server-side retention mechanism when a
   browser is slow, suspended, reconnecting, or simply fails to drain wakeups.

   These tests specify only the capacity/safety contract. They deliberately do
   not make an eviction-order guarantee: overflow is already a degraded
   presentation condition, and application correctness must be recoverable from
   authoritative state rather than from replaying this mailbox."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.client :as client]
   [manifold.stream :as s]))

(defn- test-channel
  [options]
  (client/channel
   (merge
    {:id :test/capacity
     :client (fn [_]
               {:client/scopes #{}})}
    options)))

(defn- connect!
  [channel client-id]
  (client/stream-response
   channel
   {:query-params
    {:client-id client-id}}))

(defn- close-response!
  [response]
  (when-let [stream (:body response)]
    (s/close! stream))
  nil)

(defn- eventually
  ([pred]
   (eventually pred 1000))
  ([pred timeout-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (if (pred)
         true
         (if (< (System/currentTimeMillis) deadline)
           (do
             (Thread/sleep 5)
             (recur))
           false))))))

(defn- fragments
  [n]
  (mapv
   (fn [index]
     {:test/fragment index})
   (range n)))

(deftest default-pending-mailbox-capacity-is-finite-test
  (let [configured
        (:max-pending-fragments-per-client
         client/default-options)

        channel
        (client/channel)]

    (is (and (pos-int? configured)
             (<= configured 1024))
        "The default pending mailbox must be a finite defensive entry cap, not an effectively unbounded queue.")

    (is (= configured
           (:max-pending-fragments-per-client channel))
        "The effective capacity must be explicit on the channel for diagnostics and policy review.")))

(deftest explicit-pending-capacity-bounds-one-large-send-test
  (let [capacity 3
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        response (connect! channel "client-1")
        sent (fragments 100)]
    (try
      (let [result
            (apply
             client/send-to-client!
             channel
             "client-1"
             sent)]

        (is (= 1 (:sent result))
            "Overflow is degraded ephemeral delivery, not loss of current stream ownership.")

        (is (= {"client-1" capacity}
               (client/pending-counts channel))
            "A single oversized batch must never retain more than the configured cap.")

        (is (= (- (count sent) capacity)
               (:pending-overflow-count
                (client/state-summary channel)))
            "Deliberate mailbox eviction must be observable separately from SSE wake failures.")

        (is (= 0
               (:dropped-count
                (client/state-summary channel)))
            "Capacity eviction must not masquerade as a physical stream-put failure.")

        (let [drained
              (client/drain-fragments!
               channel
               "client-1")]
          (is (= capacity (count drained)))
          (is (every? (set sent) drained)
              "The bounded mailbox may retain only work that was actually offered to this client.")
          (is (= {}
                 (client/pending-counts channel)))))
      (finally
        (close-response! response)))))

(deftest repeated-sends-cannot-grow-pending-mailbox-past-capacity-test
  (let [capacity 4
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        response (connect! channel "client-1")
        maximum-observed (atom 0)]
    (try
      (doseq [fragment (fragments 25)]
        (client/send-to-client!
         channel
         "client-1"
         fragment)

        (swap! maximum-observed
               max
               (get (client/pending-counts channel)
                    "client-1"
                    0)))

      (is (<= @maximum-observed capacity)
          "The invariant must hold after every enqueue, not only when draining.")

      (is (= capacity
             (get (client/pending-counts channel)
                  "client-1")))

      (is (= (- 25 capacity)
             (:pending-overflow-count
              (client/state-summary channel))))
      (finally
        (close-response! response)))))

(deftest reconnect-preserves-boundedness-without-resetting-pending-work-test
  (let [capacity 2
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        first-response (connect! channel "client-1")]
    (try
      (apply
       client/send-to-client!
       channel
       "client-1"
       (fragments 7))

      (is (= {"client-1" capacity}
             (client/pending-counts channel)))

      (let [replacement-response
            (connect! channel "client-1")]
        (try
          (testing "same-id reconnect keeps pending work bounded rather than multiplying ownership"
            (is (= ["client-1"]
                   (client/connected-client-ids channel)))

            (is (= {"client-1" capacity}
                   (client/pending-counts channel)))

            (apply
             client/send-to-client!
             channel
             "client-1"
             (fragments 9))

            (is (= {"client-1" capacity}
                   (client/pending-counts channel)))

            (is (= (- (+ 7 9) capacity)
                   (:pending-overflow-count
                    (client/state-summary channel)))))
          (finally
            (close-response! replacement-response))))
      (finally
        (close-response! first-response)))))

(deftest capacity-is-per-client-not-one-global-mailbox-test
  (let [capacity 2
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        response-a (connect! channel "client-a")
        response-b (connect! channel "client-b")]
    (try
      (apply
       client/broadcast!
       channel
       (fragments 5))

      (is (= {"client-a" capacity
              "client-b" capacity}
             (client/pending-counts channel))
          "One busy client must not consume another client's mailbox capacity.")

      (is (= (* 2 (- 5 capacity))
             (:pending-overflow-count
              (client/state-summary channel)))
          "Overflow diagnostics count evicted entries across independently bounded client mailboxes.")
      (finally
        (close-response! response-a)
        (close-response! response-b)))))


(deftest repeated-sends-coalesce-wakeups-until-pending-drain-test
  (let [capacity 4
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        response (connect! channel "client-1")]
    (try
      ;; Consume the connection-open wake so this test measures only wakes
      ;; created by pending-work transitions.
      (is (string?
           (deref
            (s/take! (:body response))
            1000
            nil)))

      (let [results
            (mapv
             (fn [fragment]
               (client/send-to-client!
                channel
                "client-1"
                fragment))
             (fragments 50))]

        (is (every? #(= 1 (:sent %)) results)
            "Wake coalescing must not change logical enqueue ownership.")

        (is (= 1 (reduce + (map :woke results)))
            "A continuously nonempty mailbox needs one attempted physical SSE wake, not one wake per send.")

        (is (= {"client-1" capacity}
               (client/pending-counts channel))
            "Wake coalescing composes with the independent mailbox entry bound."))

      (is (= capacity
             (count
              (client/drain-fragments!
               channel
               "client-1")))
          "Draining ends the current nonempty-mailbox wake epoch.")

      (let [after-drain
            (client/send-to-client!
             channel
             "client-1"
             {:test/fragment :after-drain})]
        (is (= 1 (:woke after-drain))
            "The first send after a destructive drain must create a fresh wake."))
      (finally
        (close-response! response)))))

(deftest wake-coalescing-is-independent-per-client-test
  (let [capacity 8
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        response-a (connect! channel "client-a")
        response-b (connect! channel "client-b")]
    (try
      (is (string?
           (deref (s/take! (:body response-a)) 1000 nil)))
      (is (string?
           (deref (s/take! (:body response-b)) 1000 nil)))

      (let [results
            (mapv
             (fn [fragment]
               (client/broadcast!
                channel
                fragment))
             (fragments 20))]
        (is (= 2 (reduce + (map :woke results)))
            "Two clients with continuously nonempty mailboxes need one wake attempt each, not forty wake attempts."))

      (client/drain-fragments! channel "client-a")

      (let [after-a-drain
            (client/broadcast!
             channel
             {:test/fragment :successor})]
        (is (= 1 (:woke after-a-drain))
            "Only the client whose mailbox became empty may need a successor wake."))
      (finally
        (close-response! response-a)
        (close-response! response-b)))))

(deftest reconnect-wake-does-not-reopen-a-redundant-send-wake-epoch-test
  (let [capacity 4
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        first-response (connect! channel "client-1")]
    (try
      (is (string?
           (deref (s/take! (:body first-response)) 1000 nil)))

      ;; Establish pending work. The replacement connection supplies its own
      ;; recovery wake. While that same pending epoch survives, later sends must
      ;; not stack additional wake attempts behind a slow reconnecting browser.
      (client/send-to-client!
       channel
       "client-1"
       {:test/fragment :before-reconnect})

      (let [replacement-response (connect! channel "client-1")]
        (try
          (is (string?
               (deref
                (s/take! (:body replacement-response))
                1000
                nil))
              "Reconnect itself always supplies the recovery wake for surviving pending work.")

          (let [results
                (mapv
                 (fn [fragment]
                   (client/send-to-client!
                    channel
                    "client-1"
                    fragment))
                 (fragments 25))]
            (is (= 0 (reduce + (map :woke results)))
                "Surviving pending work plus the reconnect-open wake must suppress redundant send wakes until drain."))

          (client/drain-fragments! channel "client-1")

          (let [after-drain
                (client/send-to-client!
                 channel
                 "client-1"
                 {:test/fragment :after-reconnect-drain})]
            (is (= 1 (:woke after-drain))))
          (finally
            (close-response! replacement-response))))
      (finally
        (close-response! first-response)))))

(deftest invalid-pending-capacities-are-rejected-at-channel-construction-test
  (doseq [invalid [nil 0 -1 1.5 "4" :four [] {}]]
    (testing (str "invalid capacity " (pr-str invalid))
      (let [error
            (try
              (client/channel
               {:max-pending-fragments-per-client invalid})
              nil
              (catch clojure.lang.ExceptionInfo ex
                ex))]
        (is (some? error)
            "Invalid capacity must fail before a channel with ambiguous retention semantics can exist.")
        (is (= :max-pending-fragments-per-client
               (:key (ex-data error))))
        (is (= invalid
               (:value (ex-data error))))))))

(deftest reset-clears-pending-capacity-state-without-changing-policy-test
  (let [capacity 2
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        first-response (connect! channel "client-1")]
    (try
      (apply
       client/send-to-client!
       channel
       "client-1"
       (fragments 7))

      (let [before-reset (client/state-summary channel)]
        (is (= {"client-1" capacity}
               (:pending-counts before-reset)))
        (is (= 5
               (:pending-overflow-count before-reset)))
        (is (= capacity
               (:max-pending-fragments-per-client before-reset))))

      (is (= :reset
             (client/reset-channel! channel)))

      (let [after-reset (client/state-summary channel)]
        (is (= 0 (:connected-count after-reset)))
        (is (= [] (:connected-client-ids after-reset)))
        (is (nil? (:latest-client-id after-reset)))
        (is (= {} (:pending-counts after-reset)))
        (is (= 0 (:sent-count after-reset)))
        (is (= 0 (:wakeup-count after-reset)))
        (is (= 0 (:dropped-count after-reset)))
        (is (= 0 (:pending-overflow-count after-reset))
            "Reset starts a fresh diagnostics epoch rather than carrying stale overflow history into a new client lifecycle.")
        (is (= capacity
               (:max-pending-fragments-per-client after-reset))
            "Reset clears runtime state, not the channel's configured retention policy."))

      (let [replacement-response (connect! channel "client-1")]
        (try
          (apply
           client/send-to-client!
           channel
           "client-1"
           (fragments 3))

          (let [summary (client/state-summary channel)]
            (is (= {"client-1" capacity}
                   (:pending-counts summary)))
            (is (= 1
                   (:pending-overflow-count summary))
                "Overflow accounting after reset must restart from zero and describe only the new lifecycle."))
          (finally
            (close-response! replacement-response))))
      (finally
        (close-response! first-response)))))


(deftest concurrent-empty-mailbox-sends-share-one-wake-edge-test
  (let [send-count 64
        capacity 128
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        response (connect! channel "client-1")
        ready (java.util.concurrent.CountDownLatch. send-count)
        start (java.util.concurrent.CountDownLatch. 1)]
    (try
      ;; Remove stream-open recovery wake from the measurement.
      (is (string?
           (deref
            (s/take! (:body response))
            1000
            nil)))

      (let [workers
            (mapv
             (fn [index]
               (future
                 (.countDown ready)
                 (.await start)
                 (client/send-to-client!
                  channel
                  "client-1"
                  {:test/concurrent-fragment index})))
             (range send-count))]

        (is (.await
             ready
             5
             java.util.concurrent.TimeUnit/SECONDS)
            "All send workers must be poised before the concurrent release.")

        (.countDown start)

        (let [results (mapv deref workers)]
          (is (every? #(= 1 (:sent %)) results)
              "Wake coalescing must not reject logically owned concurrent sends.")

          (is (= 1
                 (reduce + (map :woke results)))
              "Exactly one racing send may claim the empty -> nonempty physical wake edge.")

          (is (= {"client-1" send-count}
                 (client/pending-counts channel))
              "All concurrently owned work remains pending when the configured cap is not reached.")

          (is (= 0
                 (:pending-overflow-count
                  (client/state-summary channel))))

          (is (= send-count
                 (count
                  (client/drain-fragments!
                   channel
                   "client-1"))))

          (is (= 1
                 (:woke
                  (client/send-to-client!
                   channel
                   "client-1"
                   {:test/concurrent-fragment :successor})))
              "Destructive drain closes the contested wake epoch and permits one successor wake.")))
      (finally
        (.countDown start)
        (close-response! response)))))

(deftest rejected-coalesced-wake-preserves-bounded-work-for-reconnect-test
  (let [capacity 64
        channel (test-channel
                 {:max-pending-fragments-per-client capacity})
        response (connect! channel "client-1")
        rejected-stream (s/stream 1)]
    (try
      ;; Remove the connection-open wake, then replace the runtime owner with
      ;; an already-closed real Manifold stream. The first pending-work wake is
      ;; therefore deterministically rejected by the transport rather than by
      ;; a mocked implementation.
      (is (string?
           (deref
            (s/take! (:body response))
            1000
            nil)))

      (s/close! rejected-stream)
      (is (s/closed? rejected-stream))

      (swap!
       (:state channel)
       assoc-in
       [:clients "client-1" :stream]
       rejected-stream)

      (let [first-result
            (client/send-to-client!
             channel
             "client-1"
             {:test/fragment :first})]

        (is (= 1 (:sent first-result)))
        (is (= 1 (:woke first-result))
            "The empty -> nonempty transition owns exactly one rejected physical wake attempt.")

        (is (eventually
             #(empty?
               (client/connected-client-ids channel)))
            "Rejected coalesced wake must retire exactly that failed physical owner.")

        (is (eventually
             #(= 1
                 (:dropped-count
                  (client/state-summary channel))))
            "One rejected physical wake is one dropped wake diagnostic.")

        (is (= {"client-1" 1}
               (client/pending-counts channel))
            "Wake rejection cannot pretend the bounded pending payload was delivered.")

        (let [while-disconnected
              (client/send-to-client!
               channel
               "client-1"
               {:test/fragment :disconnected})]
          (is (= 0 (:sent while-disconnected)))
          (is (= 0 (:woke while-disconnected)))
          (is (= {"client-1" 1}
                 (client/pending-counts channel))
              "A retired physical owner cannot accumulate additional targeted work by logical id alone.")))

      (let [replacement-response (connect! channel "client-1")]
        (try
          (is (string?
               (deref
                (s/take! (:body replacement-response))
                1000
                nil))
              "Reconnect supplies the recovery wake for the surviving nonempty mailbox.")

          (let [later-results
                (mapv
                 (fn [index]
                   (client/send-to-client!
                    channel
                    "client-1"
                    {:test/fragment index}))
                 (range 25))]
            (is (every? #(= 1 (:sent %)) later-results))
            (is (= 0
                   (reduce + (map :woke later-results)))
                "Surviving failed-wake work plus reconnect recovery must suppress redundant send wakes until drain."))

          (is (= 26
                 (count
                  (client/drain-fragments!
                   channel
                   "client-1"))))

          (is (= 1
                 (:woke
                  (client/send-to-client!
                   channel
                   "client-1"
                   {:test/fragment :after-recovery-drain})))
              "Once recovered work is claimed, the next pending epoch gets exactly one fresh wake.")
          (finally
            (close-response! replacement-response))))
      (finally
        (s/close! rejected-stream)
        (close-response! response)))))
