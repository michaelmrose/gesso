(ns gesso.live.outbox.http-aleph-pipeline
  "Optional outbound Netty capture for Aleph 0.9.11 server pipelines.

   The public Aleph :http1-pipeline-transform and
   :http2-stream-pipeline-transform hooks can observe final response-frame
   ChannelPromises. Mount this observer BEFORE Aleph's request-handler (HTTP/1)
   or handler (HTTP/2): Aleph writes outward from those contexts toward the
   pipeline head. Register each Gesso response's existing v2248 Netty bridge
   before its outbound write. HTTP/1 responses are FIFO per keepalive channel;
   HTTP/2 stream channels are independently owned.

   This is a transport instrumentation component, NOT a complete Aleph server
   adapter. In particular it cannot certify that a streaming producer has
   ceased or that a listener/socket has stopped accepting. No response may be
   retired merely because a final outbound frame was observed. Only the
   caller, after confirming every provider-using producer continuation is
   quiescent, may invoke :producer-ceased! on its Netty bridge. Uninstrumented
   responses and absent final frames remain unresolved; channel close is not
   fabricated into successful final-write evidence."
  (:import [io.netty.channel Channel ChannelDuplexHandler ChannelHandlerContext
            ChannelPromise ChannelPipeline]
           [io.netty.handler.codec.http LastHttpContent]
           [io.netty.handler.codec.http2 Http2DataFrame Http2HeadersFrame]))

(defn- reject! [reason]
  (throw (ex-info "Invalid Aleph response completion capture."
                  {:error/type :gesso.live.outbox/invalid-aleph-capture
                   :reason reason})))

(def ^:private empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn- final-response-frame? [kind frame]
  (case kind
    :http1 (instance? LastHttpContent frame)
    :http2 (or (and (instance? Http2DataFrame frame)
                    (.isEndStream ^Http2DataFrame frame))
               (and (instance? Http2HeadersFrame frame)
                    (.isEndStream ^Http2HeadersFrame frame)))
    false))

(defn prepare!
  "Return :http1-pipeline-transform, :http2-stream-pipeline-transform,
   :register-response! and :stats.

   For each accepted Aleph Ring request, call :register-response! with
   (:aleph/channel request) and the per-response result of
   http-netty-terminal/prepare! BEFORE allowing Aleph to write its response.
   The :aleph/channel key is internal in Aleph 0.9.11; other Aleph versions
   require requalification. Call the bridge's :producer-ceased! separately
   when its producer and callbacks are verifiably quiescent; do not invoke it
   on Ring return or upstream Manifold close by default.

   :http1-pipeline-transform attaches before the named request-handler;
   :http2-stream-pipeline-transform attaches before the named handler.
   The capture is per physical Channel, not per HTTP/2 parent connection.
   A FIFO protects pipelined HTTP/1 final-write pairing.

   :max-pending-per-channel (1..1024, default 128) bounds registrations.
   An unmatched Aleph-generated response (e.g. rejection/continue) is
   forwarded without inventing a matching Gesso response. Channel inactivity
   drops capture-side references but increments :orphaned; this does NOT
   retire the independently owned Gesso listener obligation."
  ([] (prepare! {}))
  ([{:keys [max-pending-per-channel] :as options}]
   (when-not (and (map? options)
                  (every? #{:max-pending-per-channel} (keys options)))
     (reject! :invalid-options))
   (let [limit (get options :max-pending-per-channel 128)]
     (when-not (and (integer? limit) (<= 1 limit 1024))
       (reject! :invalid-pending-limit))
     (let [guard (Object.)
           state (atom {:channels {} :registered 0 :matched 0
                        :unmatched 0 :orphaned 0})]
       (letfn [(enqueue! [^Channel channel terminal-bridge]
                 (when-not (instance? Channel channel)
                   (reject! :invalid-channel))
                 (when-not (and (map? terminal-bridge)
                                (fn? (:watch-final-write! terminal-bridge)))
                   (reject! :invalid-terminal-bridge))
                 (locking guard
                   (let [q (get-in @state [:channels channel] empty-queue)]
                     (when (>= (count q) limit)
                       (reject! :pending-channel-capacity))
                     (swap! state
                            (fn [s] (-> s
                                        (assoc-in [:channels channel]
                                                  (conj q terminal-bridge))
                                        (update :registered inc))))))
                 :registered)
               (observe-final! [^Channel channel ^ChannelPromise promise]
                 (let [bridge
                       (locking guard
                         (let [q (get-in @state [:channels channel] empty-queue)]
                           (if (empty? q)
                             (do (swap! state update :unmatched inc) nil)
                             (let [item (peek q)
                                   remaining (pop q)]
                               (swap! state
                                      (fn [s]
                                        (-> s
                                            (update :matched inc)
                                            (cond-> (empty? remaining)
                                              (update :channels dissoc channel)
                                              (seq remaining)
                                              (assoc-in [:channels channel]
                                                        remaining)))))
                               item))))]
                   (when bridge
                     ;; The actual ChannelPromise is the Netty future. The
                     ;; bridge still requires independent producer quiescence.
                     ((:watch-final-write! bridge) promise))))
               (channel-gone! [^Channel channel]
                 (locking guard
                   (let [q (get-in @state [:channels channel] empty-queue)]
                     (swap! state
                            (fn [s] (-> s
                                        (update :channels dissoc channel)
                                        (update :orphaned + (count q))))))))
               (instrument [^ChannelPipeline pipeline kind before handler-name]
                 (when-not (.get pipeline ^String before)
                   (reject! :missing-aleph-handler))
                 (.addBefore pipeline ^String before ^String handler-name
                             (proxy [ChannelDuplexHandler] []
                               (write [^ChannelHandlerContext ctx msg
                                       ^ChannelPromise promise]
                                 (when (final-response-frame? kind msg)
                                   (observe-final! (.channel ctx) promise))
                                 (.write ctx msg promise))
                               (channelInactive [^ChannelHandlerContext ctx]
                                 (channel-gone! (.channel ctx))
                                 (.fireChannelInactive ctx))))
                 pipeline)]
         {:register-response! enqueue!
          :http1-pipeline-transform
          (fn [pipeline]
            (instrument pipeline :http1 "request-handler"
                        "gesso-final-http1"))
          :http2-stream-pipeline-transform
          (fn [pipeline]
            (instrument pipeline :http2 "handler" "gesso-final-http2"))
          :stats (fn [] (let [snapshot @state]
                          (-> (dissoc snapshot :channels)
                              (assoc :pending
                                     (reduce + 0 (map count
                                                      (vals (:channels snapshot))))))))})))))
