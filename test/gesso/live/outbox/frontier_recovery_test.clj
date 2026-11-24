(ns gesso.live.outbox.frontier-recovery-test
  "Physical recovery after a submitted acknowledgment loses its reply."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.reconciliation :as reconciliation]
            [gesso.live.outbox.recovery-audit :as audit]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util UUID]))

(def ^:private source-id "gesso/test-frontier-recovery")

(defn- terminal-write-for? [intent-id ops]
  (some (fn [op]
          (and (vector? op)
               (= :put-docs (first op))
               (= outbox/intent-table (second op))
               (some #(and (= intent-id (:xt/id %))
                           (= :dispatch-accepted (:gesso.live.outbox/status %)))
                     (drop 2 op))))
        ops))

(deftest submitted-receipt-survives-cycle-interruption-and-rescan-test
  (with-open [node (xtn/start-node {})]
    (let [a (UUID/fromString "00000000-0000-0000-0000-000000000001")
          b (UUID/fromString "00000000-0000-0000-0000-000000000002")
          ids [a b]
          subjects (mapv #(str "request-" %) ids)
          plan (outbox/prepare!
                {:intent-ids ids :source-id source-id
                 :operation :request/claim :purpose :gesso.live/refresh}
                (mapv #(hash-map :topic :request :id % :change/kind :updated) subjects))
          system (live/create {:rules [{:when-topic :request
                                       :expand (fn [_ change]
                                                 [(select-keys change [:topic :id :change/kind])])}]
                               :source-options {:coalesce-window-ms 0}
                               :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}})
          taps (mapv #(source/subscribe (:source system) {:topic :request :id %}) subjects)
          original-execute live.xtdb/execute-tx!
          lost (ex-info "terminal transaction submitted; reply lost" {:test/lost-reply true})
          submissions (atom 0)]
      (try
        ;; Neither asynchronous submission is explicitly awaited by the test.
        (xt/submit-tx node (conj (vec (:guard-ops plan)) (:tx-op plan)))
        (let [failure (with-redefs [live.xtdb/execute-tx!
                                    (fn [n ops & options]
                                      (if (terminal-write-for? b ops)
                                        (do (swap! submissions inc)
                                            (xt/submit-tx n ops)
                                            (throw lost))
                                        (apply original-execute n ops options)))]
                        (try (cycle/process-cycle! node system {} source-id
                                                   {:worker-id "original" :max-items 2})
                             nil
                             (catch clojure.lang.ExceptionInfo e e)))
              data (ex-data failure)
              observed (reconciliation/inspect! node source-id data)
              page (audit/inspect-source-page! node {:source-id source-id})
              expected-receipts [(get-in data [:completed 0 :receipt-id])
                                 (get-in data [:in-flight :receipt-id])]]
          (is (= 1 @submissions))
          (is (= :gesso.live.outbox/cycle-interrupted (:error/type data)))
          (is (= :gesso.live.outbox/post-dispatch-ack-unknown (:original-error/type data)))
          (is (identical? lost (some-> failure .getCause .getCause)))
          (is (= 1 (:completed-count data)))
          (is (= a (get-in data [:completed 0 :intent-id])))
          (is (= b (get-in data [:in-flight :intent-id])))
          (is (= :unverified-post-dispatch-receipt (get-in data [:in-flight :receipt-evidence])))
          (is (= :matching-acknowledgment-recorded (:status observed)))
          (is (= [:matching-acknowledgment-recorded] (mapv :status (:completed-observations observed))))
          (is (true? (:rescan-required? observed)))
          (is (not (contains? observed :may-replay?)))
          (is (= ids (mapv :intent-id (:entries page))))
          (is (= [:acknowledgment-recorded :acknowledgment-recorded]
                 (mapv :observation (:entries page))))
          (is (every? uuid? expected-receipts))
          (is (= expected-receipts (mapv :receipt-id (:entries page))))
          (is (= subjects (mapv #(:id (deref (stream/take! %) 1500 ::timeout)) taps)))
          (let [rescan (cycle/process-cycle! node system {} source-id
                                             {:worker-id "successor" :max-items 2})]
            (is (= :cycle-completed (:status rescan)))
            (is (= :observed-end (:halt-reason rescan)))
            (is (zero? (:processed rescan)))
            (is (every? #(= ::timeout (deref (stream/take! %) 100 ::timeout)) taps))))
        (finally
          (doseq [tap taps] (stream/close! tap))
          (live/close! system))))))
