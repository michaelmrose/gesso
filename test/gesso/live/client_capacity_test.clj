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
