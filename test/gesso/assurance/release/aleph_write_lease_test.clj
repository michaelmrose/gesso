(ns gesso.assurance.release.aleph-write-lease-test
  "v2273: Permanent opt-in Netty final-write ownership qualification for the
   release traffic fence. Exercise original final-frame capture through real
   Netty EmbeddedChannel promises, preserving explicit differences between
   producer quiescence, write disposition, delivery, and release authority.
   These tests do NOT qualify a deployed Aleph listener, physical socket
   write, installed Biff adapter, arbitrary asynchronous work, or SSE."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.release.traffic-fence :as fence]
            [gesso.assurance.release.aleph-write-lease :as lease])
  (:import [io.netty.channel ChannelDuplexHandler ChannelHandlerContext ChannelPromise]
           [io.netty.channel.embedded EmbeddedChannel]
           [io.netty.buffer Unpooled]
           [io.netty.handler.codec.http DefaultHttpContent DefaultLastHttpContent]
           [io.netty.handler.codec.http2 DefaultHttp2DataFrame]
           [java.io ByteArrayInputStream]))

(defn- refused [f]
  (try (f) :did-not-refuse
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) :unknown-ex-info))))

(defn- new-setup
  ([] (new-setup :http1 (constantly {:status 200 :headers {"content-type" "text/plain"} :body "OK"}) true))
  ([kind business allowed?]
   (let [f (fence/issue-local-ring-traffic-fence!
            {:qualify! (constantly true) :drain-timeout-ms 8})
         l (lease/prepare! f)
         token ((:prepare! f) {:generation :test/one
                               :handler ((:wrap-handler l) business)
                               :guard! (constantly allowed?)})
         _ ((:activate! f) token)
         ch (EmbeddedChannel.)
         pipeline (.pipeline ch)
         name (if (= :http1 kind) "request-handler" "handler")]
     (.addLast pipeline name (proxy [ChannelDuplexHandler] []))
     ((if (= :http1 kind) (:http1-pipeline-transform l)
          (:http2-stream-pipeline-transform l)) pipeline)
     {:fence f :lease l :token token :ch ch :handler name})))

(defn- invoke [{:keys [fence ch]} & [uri]]
  ((:handler fence) {:aleph/channel ch :uri (or uri "/x")}))

(defn- outbound [{:keys [ch handler]} msg]
  (let [f (.writeAndFlush (.context (.pipeline ch) handler) msg)]
    (.runPendingTasks ch)
    f))

(defn- complete-http1 [setup]
  (outbound setup (DefaultLastHttpContent.)))

(deftest exact-final-write-releases-retained-lease
  (let [{:keys [fence lease token ch] :as s} (new-setup)]
    (try
      (is (= 200 (:status (invoke s))))
      (is (= 1 (:retained-continuations ((:summary! fence)))))
      (is (= 1 (:pending-registered ((:stats lease)))))
      (is (= :registered-continuation-drain-timeout (refused #((:retire! fence) token))))
      (is (= :draining (:phase ((:summary! fence)))))
      (is (= 503 (:status (invoke s))))
      (is (true? (.isSuccess (complete-http1 s))))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (is (= 0 (:pending-registered ((:stats lease)))))
      (is (= 1 (get-in ((:stats lease)) [:counts :succeeded])))
      (is (= :retired (:status ((:retire! fence) token))))
      (is (= :none (:release-authority ((:stats lease)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest not-final-frame-cannot-release-lease
  (let [{:keys [ch fence lease] :as s} (new-setup)]
    (try
      (is (= 200 (:status (invoke s))))
      (is (.isSuccess (outbound s (DefaultHttpContent. Unpooled/EMPTY_BUFFER))))
      (is (= 1 (:retained-continuations ((:summary! fence)))))
      (is (= 1 (:pending-registered ((:stats lease)))))
      (is (= 0 (get-in ((:stats lease)) [:capture :matched])))
      (is (.isSuccess (complete-http1 s)))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest ordered-http1-responses-retire-separately
  (let [{:keys [ch fence lease] :as s} (new-setup)]
    (try
      (invoke s "/first")
      (invoke s "/second")
      (is (= 2 (:retained-continuations ((:summary! fence)))))
      (is (.isSuccess (complete-http1 s)))
      (is (= 1 (:retained-continuations ((:summary! fence)))))
      (is (= 1 (:pending-registered ((:stats lease)))))
      (is (.isSuccess (complete-http1 s)))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (is (= 2 (get-in ((:stats lease)) [:capture :matched])))
      (finally (.finishAndReleaseAll ch)))))

(deftest http2-endstream-is-final-write
  (let [{:keys [ch fence lease] :as s}
        (new-setup :http2 (constantly {:status 200 :headers {} :body "H2"}) true)]
    (try
      (is (= 200 (:status (invoke s))))
      (is (.isSuccess (outbound s (DefaultHttp2DataFrame. Unpooled/EMPTY_BUFFER false))))
      (is (= 1 (:retained-continuations ((:summary! fence)))))
      (is (.isSuccess (outbound s (DefaultHttp2DataFrame. Unpooled/EMPTY_BUFFER true))))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (is (= 1 (get-in ((:stats lease)) [:capture :matched])))
      (finally (.finishAndReleaseAll ch)))))

(deftest absent-aleph-channel-must-never-generate-a-lease
  (let [{:keys [ch fence lease] :as s} (new-setup)]
    (try
      (is (= :active-aleph-request-channel-required
             (refused #((:handler fence) {:uri "/no-channel"}))))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (is (= 0 (:pending-registered ((:stats lease)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest no-write-does-not-become-fabricated-completion-on-close
  (let [{:keys [ch fence lease] :as s} (new-setup)]
    (invoke s)
    (.finishAndReleaseAll ch)
    (is (= 1 (:retained-continuations ((:summary! fence)))))
    (is (= 1 (:pending-registered ((:stats lease)))))
    (is (= :registered-continuation-drain-timeout
           (refused #((:close! fence)))))))

(deftest deferred-body-does-not-receive-premature-terminal-evidence
  (let [{:keys [ch fence lease] :as s}
        (new-setup :http1 (constantly {:status 200 :headers {}
                                      :body (ByteArrayInputStream. (byte-array [1 2]))})
                   true)]
    (try
      (is (= 503 (:status (invoke s))))
      (is (= 1 (:retained-continuations ((:summary! fence)))))
      (is (= 0 (:pending-registered ((:stats lease)))))
      (is (= 1 (get-in ((:stats lease)) [:counts :handler-uncertain])))
      (finally (.finishAndReleaseAll ch)))))

(deftest guard-refusal-avoids-lease-or-business-entry
  (let [hits (atom 0)
        {:keys [fence lease ch] :as s}
        (new-setup :http1 (fn [_] (swap! hits inc)
                            {:status 200 :headers {} :body "hello"}) false)]
    (try
      (is (= 503 (:status (invoke s))))
      (is (= 0 @hits))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (is (= 0 (:pending-registered ((:stats lease)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest omitted-transform-leaves-lease-outstanding
  (let [{:keys [ch fence lease] :as s} (new-setup)]
    (try
      (invoke s)
      (.remove (.pipeline ch) "gesso-final-http1")
      (is (.isSuccess (complete-http1 s)))
      (is (= 1 (:retained-continuations ((:summary! fence)))))
      (is (= 1 (:pending-registered ((:stats lease)))))
      (is (= :registered-continuation-drain-timeout
             (refused #((:close! fence)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest invalid-provider-and-handler-are-rejected
  (is (= :issued-continuation-fence-required (refused #(lease/prepare! {}))))
  (let [{:keys [lease ch] :as s} (new-setup)]
    (try
      (is (= :original-ring-handler-required
             (refused #((:wrap-handler lease) nil))))
      (finally (.finishAndReleaseAll ch)))))

(deftest outside-fence-wrapper-refuses-before-business-work
  (let [hits (atom 0)
        s (new-setup :http1 (fn [_] (swap! hits inc)
                             {:status 200 :headers {} :body "danger"}) true)
        {:keys [ch lease]} s
        unissued ((:wrap-handler lease) (fn [_] (swap! hits inc)
                                           {:status 200 :headers {} :body "bad"}))]
    (try
      (is (= :no-current-admitted-request
             (refused #(unissued {:aleph/channel ch :uri "/external"}))))
      (is (= 0 @hits))
      (is (= 0 (:pending-registered ((:stats lease)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest throwing-business-handler-retains-uncertain-lease
  (let [{:keys [ch lease fence] :as s}
        (new-setup :http1 (fn [_] (throw (ex-info "handler error" {:handler true}))) true)]
    (try
      (is (= :handler-error
             (try (invoke s) :not-thrown
                  (catch clojure.lang.ExceptionInfo e
                    (if (:handler (ex-data e)) :handler-error :wrong-error)))))
      (is (= 1 (:retained-continuations ((:summary! fence)))))
      (is (= 1 (get-in ((:stats lease)) [:counts :handler-uncertain])))
      (is (= :registered-continuation-drain-timeout
             (refused #((:close! fence)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest real-netty-final-promise-failure-releases-but-does-not-claim-delivery
  (let [{:keys [ch fence lease token] :as s} (new-setup)]
    (try
      (.addFirst (.pipeline ch) "write-failure"
                 (proxy [ChannelDuplexHandler] []
                   (write [^ChannelHandlerContext ctx msg ^ChannelPromise promise]
                     (io.netty.util.ReferenceCountUtil/release msg)
                     (.setFailure promise (IllegalStateException. "forced write failure")))))
      (is (= 200 (:status (invoke s))))
      (is (false? (.isSuccess (complete-http1 s))))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (is (= 1 (get-in ((:stats lease)) [:counts :failed])))
      (is (= :none (:release-authority ((:stats lease)))))
      (is (= :retired (:status ((:retire! fence) token))))
      (finally (.finishAndReleaseAll ch)))))

(deftest bounded-capture-registration-refusal-does-not-create-129th-lease
  (let [{:keys [ch fence lease] :as s} (new-setup)]
    (try
      (dotimes [_ 128] (is (= 200 (:status (invoke s)))))
      (is (= 128 (:retained-continuations ((:summary! fence)))))
      (is (= :pending-channel-capacity (refused #(invoke s))))
      (is (= 128 (:retained-continuations ((:summary! fence)))))
      (is (= 128 (:pending-registered ((:stats lease)))))
      (dotimes [_ 128] (complete-http1 s))
      (is (= 0 (:retained-continuations ((:summary! fence)))))
      (is (= 0 (:pending-registered ((:stats lease)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest malformed-channels-refuse-before-business-execution
  (let [hits (atom 0)
        {:keys [ch fence lease] :as s}
        (new-setup :http1 (fn [_] (swap! hits inc)
                            {:status 200 :body "entered"}) true)]
    (try
      (doseq [invalid [nil false :bad (Object.) 42 [] {}]]
        (is (= :active-aleph-request-channel-required
               (refused #((:handler fence) {:aleph/channel invalid}))))
        (is (zero? @hits))
        (is (zero? (:retained-continuations ((:summary! fence))))))
      (.finishAndReleaseAll ch)
      (is (= :active-aleph-request-channel-required
             (refused #((:handler fence) {:aleph/channel ch}))))
      (is (zero? @hits))
      (is (zero? (:pending-registered ((:stats lease)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest mutable-byte-array-body-is-snapshotted-before-ring-return
  (let [mutable (byte-array [10 20 30])
        {:keys [ch fence lease] :as s}
        (new-setup :http1 (constantly {:status 200 :headers {}
                                      :body mutable}) true)]
    (try
      (let [response (invoke s)
            returned (:body response)]
        (is (instance? (Class/forName "[B") returned))
        (is (= [10 20 30] (mapv int returned)))
        (is (not (identical? mutable returned)))
        (aset-byte mutable 0 (byte 99))
        (is (= [10 20 30] (mapv int returned)))
        (is (= 1 (:retained-continuations ((:summary! fence)))))
        (is (= 1 (:pending-registered ((:stats lease)))))
        (is (.isSuccess (complete-http1 s)))
        (is (= 0 (:retained-continuations ((:summary! fence)))))
        (is (= 1 (get-in ((:stats lease)) [:counts :succeeded]))))
      (finally (.finishAndReleaseAll ch)))))

(deftest unsupported-response-envelope-retains-unknown-owner
  (doseq [bad [nil [] false
               {:status 200 :headers {} :body (Object.)}
               {:status 200 :headers {} :body "ok" :on-close (fn [] nil)}
               {:status 200 :headers {"test" 1} :body "ok"}
               (with-meta {:status 200 :body "ok"} {:hidden true})]]
    (let [hits (atom 0)
          {:keys [ch fence lease] :as s}
          (new-setup :http1 (fn [_] (swap! hits inc) bad) true)]
      (try
        (is (= 503 (:status (invoke s))))
        (is (= 1 @hits))
        (is (= 1 (:retained-continuations ((:summary! fence)))))
        (is (= 0 (:pending-registered ((:stats lease)))))
        (is (= 1 (get-in ((:stats lease)) [:counts :handler-uncertain])))
        (is (= :registered-continuation-drain-timeout
               (refused #((:close! fence)))))
        (finally (.finishAndReleaseAll ch))))))

(deftest returned-evidence-stays-non-authoritative-before-and-after-write
  (let [{:keys [ch lease fence token] :as s} (new-setup)]
    (try
      (doseq [stats [((:stats lease))]]
        (is (= lease/report-type (:type stats)))
        (is (= lease/report-version (:version stats)))
        (is (= :one-host-finite-ring-aleph-write-only (:scope stats)))
        (is (= :never-implies-completion (:missing-final-frame stats)))
        (is (= :never-implies-write-disposition (:channel-close stats)))
        (is (= :none (:release-authority stats)))
        (is (= :not-issued (:activation-eligibility stats)
               (:completion-claim stats))))
      (invoke s)
      (is (= 1 (:pending-registered ((:stats lease)))))
      (is (= :registered-continuation-drain-timeout
             (refused #((:retire! fence) token))))
      (is (.isSuccess (complete-http1 s)))
      (let [stats ((:stats lease))]
        (is (zero? (:pending-registered stats)))
        (is (= 1 (get-in stats [:counts :succeeded])))
        (is (= :none (:release-authority stats)))
        (is (= :not-issued (:activation-eligibility stats)
               (:completion-claim stats)))
        (is (= :retired (:status ((:retire! fence) token)))))
      (finally (.finishAndReleaseAll ch)))))

(deftest channel-close-retains-unresolved-lease-and-records-orphan
  (let [{:keys [ch fence lease] :as s} (new-setup)]
    (invoke s)
    (is (= 1 (:pending-registered ((:stats lease)))))
    (.finishAndReleaseAll ch)
    (is (= 1 (:retained-continuations ((:summary! fence)))))
    (is (= 1 (:pending-registered ((:stats lease)))))
    (is (= 1 (get-in ((:stats lease)) [:capture :orphaned])))
    (is (= 0 (get-in ((:stats lease)) [:counts :succeeded])))
    (is (= :registered-continuation-drain-timeout
           (refused #((:close! fence)))))))

(deftest final-write-promise-must-settle-before-continuation-finishes
  ;; Netty's producer-ceased event alone does not retire a lease. Hold an
  ;; outbound promise pending *after* the terminal frame reaches the installed
  ;; pipeline observer, then settle it explicitly. This is an embedded Netty
  ;; promise harness, not a claim of real socket delivery.
  (let [{:keys [ch fence lease token] :as s} (new-setup)
        promise-ref (atom nil)]
    (try
      (.addFirst (.pipeline ch) "deferred-final-write"
                 (proxy [ChannelDuplexHandler] []
                   (write [^ChannelHandlerContext _ctx msg ^ChannelPromise promise]
                     (io.netty.util.ReferenceCountUtil/release msg)
                     (reset! promise-ref promise))))
      (is (= 200 (:status (invoke s))))
      (let [write-promise (complete-http1 s)]
        (is (identical? write-promise @promise-ref))
        (is (false? (.isDone write-promise)))
        (is (= 1 (:retained-continuations ((:summary! fence)))))
        (is (= 1 (:pending-registered ((:stats lease)))))
        (is (= :registered-continuation-drain-timeout
               (refused #((:retire! fence) token))))
        (is (= :draining (:phase ((:summary! fence)))))
        (is (= 503 (:status (invoke s))))
        (.setSuccess ^ChannelPromise @promise-ref)
        (.runPendingTasks ch)
        (is (.isSuccess write-promise))
        (is (= 0 (:retained-continuations ((:summary! fence)))))
        (is (= 0 (:pending-registered ((:stats lease)))))
        (is (= 1 (get-in ((:stats lease)) [:counts :succeeded])))
        (is (= :retired (:status ((:retire! fence) token)))))
      (finally (.finishAndReleaseAll ch)))))
