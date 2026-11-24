(ns gesso.assurance.release.traffic-fence-test
  "v2271: Permanent adversarial qualification of the host-local Ring fence.

   Exercise the enforcing handler, exactly-once opaque continuation leases,
   bounded retention and staged candidates, fail-closed timed drain, and
   cross-thread completion while the retirement write lock is held.
   These tests establish neither authentic release eligibility nor automatic
   observation of hidden background work, SSE, or physical wire completion."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.release.traffic-fence :as fence])
  (:import [java.io ByteArrayInputStream]
           [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private unavailable-status 503)
(def ^:private response {:status 200 :headers {"content-type" "text/plain"}
                         :body "ok"})

(defn- reason [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (if (:gesso.release.traffic-fence/rejection (ex-data e))
           (:reason (ex-data e))
           [:unexpected-ex-info (ex-data e)]))))

(defn- issue!
  ([] (issue! {}))
  ([opts]
   (fence/issue-local-ring-traffic-fence!
    (merge {:qualify! (constantly true) :drain-timeout-ms 80} opts))))

(defn- prepare!
  ([f gen handler] (prepare! f gen handler (constantly true)))
  ([f gen handler guard!]
   ((:prepare! f) {:generation gen :handler handler :guard! guard!})))

(defn- activate!
  ([f gen handler] (activate! f gen handler (constantly true)))
  ([f gen handler guard!]
   (let [token (prepare! f gen handler guard!)]
     ((:activate! f) token)
     token)))

(defn- denied? [reply] (= unavailable-status (:status reply)))
(defn- down! [^CountDownLatch latch] (.countDown latch))
(defn- reached? [^CountDownLatch latch] (.await latch 2 TimeUnit/SECONDS))
(defn- daemon-task [f]
  (let [result (promise)
        worker (Thread.
                (fn [] (deliver result
                                (try {:value (f)}
                                     (catch Throwable t {:error t})))))]
    (.setDaemon worker true)
    (.start worker)
    {:worker worker :result result}))
(defn- completion [task]
  (let [r (deref (:result task) 5000 ::timed-out)]
    (if (= ::timed-out r) r
        (if-let [error (:error r)] (throw error) (:value r)))))

(defn- no-authority! [summary]
  (is (= fence/report-type (:type summary)))
  (is (= fence/report-version (:version summary)))
  (is (= :one-host-synchronous-ring-entrypoint-only (:scope summary)))
  (is (= :trusted-host-callback-conditional
         (:host-qualifier-provenance summary)))
  (is (= :registered-continuations-only (:async-response-drain summary)))
  (is (= :not-established (:unregistered-transferred-work summary)))
  (is (= :not-established (:physical-wire-completion summary)))
  (is (= :none (:release-authority summary)))
  (is (= :none (:runtime-effect-permission summary)))
  (doseq [k [:installation-eligibility :activation-eligibility
             :completion-claim]]
    (is (= :not-issued (get summary k)))))

(deftest inactive-and-closed-fence-deny-real-handler-entry
  (let [hits (atom 0)
        f (issue!)
        handler (fn [_] (swap! hits inc) response)]
    (no-authority! ((:summary! f)))
    (is (= :inactive (:phase ((:summary! f)))))
    (is (denied? ((:handler f) {})))
    (let [token (activate! f :test/first handler)]
      (is (= response ((:handler f) {:uri "/"})))
      (is (= 1 @hits))
      (is (= :active (:phase ((:summary! f)))))
      (is (= :test/first (:selected-generation ((:summary! f)))))
      (is (= :retired (:status ((:retire! f) token))))
      (is (denied? ((:handler f) {})))
      (is (= 1 @hits))
      (is (= :closed (:status ((:close! f)))))
      (is (= :closed (:status ((:close! f)))))
      (is (denied? ((:handler f) {})))
      (is (= :permanently-closed
             (reason #(prepare! f :test/after handler))))
      (no-authority! ((:summary! f))))))

(deftest malformed-construction-and-candidates-rejected-before-qualification
  (doseq [invalid [nil [] {} {:qualify! true} {:qualify! nil}
                   {:qualify! (constantly true) :drain-timeout-ms 0}
                   {:qualify! (constantly true) :drain-timeout-ms 60001}
                   {:qualify! (constantly true) :drain-timeout-ms 1.5}
                   {:qualify! (constantly true) :unexpected true}
                   (with-meta {:qualify! (constantly true)} {:unsafe true})]]
    (is (= :trusted-host-qualifier-required
           (reason #(fence/issue-local-ring-traffic-fence! invalid)))))
  (let [qualification-calls (atom 0)
        f (issue! {:qualify! (fn [_] (swap! qualification-calls inc) true)})]
    (doseq [bad [nil [] {} {:generation :test/g :handler identity}
                 {:generation :test/g :handler identity :guard! nil}
                 {:generation :unqualified :handler identity :guard! identity}
                 {:generation :test/g :handler 5 :guard! identity}
                 {:generation :test/g :handler identity :guard! identity
                  :extra true}
                 (with-meta {:generation :test/g :handler identity
                             :guard! identity} {:hidden true})]]
      (is (= :invalid-immutable-dispatch-candidate
             (reason #((:prepare! f) bad)))))
    (is (zero? @qualification-calls))
    (is (= 0 (:staged-count ((:summary! f)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest qualification-sees-exact-functions-and-does-not-grant-a-credential
  (let [seen (atom [])
        handler (fn [_] response)
        guard! (constantly true)
        f (issue! {:qualify! (fn [x] (swap! seen conj x)
                                   (and (identical? handler (:candidate-handler x))
                                        (identical? guard! (:candidate-guard! x))))})
        token (prepare! f :test/exact handler guard!)
        activated ((:activate! f) token)]
    (is (= :host-selected-only (:status activated)))
    (is (= :none (:release-authority activated)))
    (is (= :not-issued (:activation-eligibility activated)))
    (is (= :test/exact (:candidate-generation (first @seen))))
    (is (= 0 (:expected-epoch (first @seen))))
    (is (= :one-host-synchronous-ring-entrypoint-only (:scope (first @seen))))
    (is (= response ((:handler f) {})))
    (is (= :unknown-or-consumed-candidate (reason #((:activate! f) token))))
    (is (= :closed (:status ((:close! f)))))))

(deftest failed-qualification-consumes-candidate-and-preserves-serving-generation
  (let [allow? (atom true)
        hits (atom [])
        f (issue! {:qualify! (fn [_] @allow?)})
        old-token (activate! f :test/old
                             (fn [_] (swap! hits conj :old) response))]
    (reset! allow? false)
    (let [next-token (prepare! f :test/new (fn [_] {:status 201 :body "new"}))]
      (is (= :qualification-not-established
             (reason #((:activate! f) next-token))))
      (is (= :unknown-or-consumed-candidate
             (reason #((:activate! f) next-token))))
      (is (= :test/old (:selected-generation ((:summary! f)))))
      (is (= response ((:handler f) {})))
      (is (= [:old] @hits))
      (is (= :unknown-or-consumed-candidate
             (reason #((:cancel! f) next-token)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest exact-true-qualification-and-guard-are-required
  (doseq [value [false nil :yes 1 (Object.)]]
    (let [hits (atom 0)
          f (issue! {:qualify! (constantly value)})
          token (prepare! f :test/not-true
                          (fn [_] (swap! hits inc) response))]
      (is (= :qualification-not-established (reason #((:activate! f) token))))
      (is (denied? ((:handler f) {})))
      (is (zero? @hits))
      (is (= :closed (:status ((:close! f)))))))
  (let [f (issue! {:qualify! (fn [_] (throw (Exception. "down")))})
        token (prepare! f :test/offline (constantly response))]
    (is (= :qualification-unavailable (reason #((:activate! f) token))))
    (is (= :closed (:status ((:close! f))))))
  (doseq [value [false nil :yes 1]]
    (let [f (issue!)
          calls (atom 0)
          token (activate! f :test/no-guard
                           (fn [_] (swap! calls inc) response)
                           (constantly value))]
      (is (denied? ((:handler f) {})))
      (is (zero? @calls))
      (is (= :closed (:status ((:close! f)))))))
  (let [f (issue!)
        token (activate! f :test/throws
                         (constantly response)
                         (fn [_] (throw (Exception. "guard failed"))))]
    (is (denied? ((:handler f) {})))
    (is (= :closed (:status ((:close! f)))))))

(deftest reentrant-qualification-and-protected-handler-lifecycle-are-rejected
  (let [f-ref (atom nil)
        callbacks (atom [])
        f (issue! {:qualify! (fn [_]
                              (swap! callbacks conj
                                     (reason #((:close! @f-ref))))
                              (swap! callbacks conj
                                     (reason #(prepare! @f-ref :test/injected
                                                        (constantly response))))
                              (swap! callbacks conj
                                     (:status ((:handler @f-ref) {})))
                              true)})]
    (reset! f-ref f)
    (let [token (activate! f :test/reentrant
                           (fn [_]
                             {:status 200
                              :body (str (reason #((:close! f))) "/"
                                         (reason #(prepare! f :test/inside
                                                            (constantly response))))}))]
      (is (= [:reentrant-lifecycle-transition
              :reentrant-lifecycle-transition 503] @callbacks))
      (is (= (str :lifecycle-transition-during-protected-request "/"
                  :lifecycle-transition-during-protected-request)
             (:body ((:handler f) {}))))
      (is (= :active (:phase ((:summary! f)))))
      (is (= :closed (:status ((:close! f))))))))

(deftest candidate-identity-and-generation-history-are-not-reusable
  (let [f (issue!)
        staged (prepare! f :test/staged (constantly response))]
    (is (= :reused-or-already-staged-generation
           (reason #(prepare! f :test/staged (constantly response)))))
    (is (= :unknown-or-consumed-candidate (reason #((:cancel! f) (Object.)))))
    (is (= :cancelled (:status ((:cancel! f) staged))))
    (is (= :unknown-or-consumed-candidate (reason #((:cancel! f) staged))))
    (let [t (activate! f :test/staged (constantly response))]
      (is (= :reused-or-already-staged-generation
             (reason #(prepare! f :test/staged (constantly response)))))
      (is (= :retired (:status ((:retire! f) t))))
      (is (= :reused-or-already-staged-generation
             (reason #(prepare! f :test/staged (constantly response)))))
      (is (= :not-the-current-active-generation
             (reason #((:retire! f) t)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest staging-inventory-is-bounded-and-cancellable
  (let [f (issue!)
        tokens (mapv (fn [i]
                       (prepare! f (keyword "test" (str "staged-" i))
                                 (constantly response)))
                     (range 64))]
    (is (= 64 (:staged-count ((:summary! f)))))
    (is (= :too-many-staged-generations
           (reason #(prepare! f :test/overflow (constantly response)))))
    (is (= :cancelled (:status ((:cancel! f) (first tokens)))))
    (is (= 63 (:staged-count ((:summary! f)))))
    (is (some? (prepare! f :test/next (constantly response))))
    (is (= 64 (:staged-count ((:summary! f)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest continuation-registration-only-during-admitted-handler
  (let [f (issue!)
        captured (atom nil)
        before (reason #((:retain-current! f)))
        token (activate! f :test/lease
                         (fn [_]
                           (reset! captured ((:retain-current! f)))
                           response))]
    (is (= :no-current-admitted-request before))
    (is (= response ((:handler f) {})))
    (is (= 1 (:retained-continuations ((:summary! f)))))
    (is (= :no-current-admitted-request (reason #((:retain-current! f)))))
    (is (= :finished (:status ((:finish! f) @captured))))
    (is (= 0 (:retained-continuations ((:summary! f)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest continuation-tokens-are-identity-scoped-and-once-only
  (let [f (issue!) other (issue!)
        leases (atom [])
        token (activate! f :test/one
                         (fn [_] (swap! leases conj ((:retain-current! f))) response))]
    ((:handler f) {})
    ((:handler f) {})
    (is (= 2 (:retained-continuations ((:summary! f)))))
    (is (= :unknown-or-already-finished-continuation
           (reason #((:finish! other) (first @leases)))))
    (is (= :unknown-or-already-finished-continuation
           (reason #((:finish! f) nil))))
    (is (= :unknown-or-already-finished-continuation
           (reason #((:finish! f) (Object.)))))
    (is (= :finished (:status ((:finish! f) (first @leases)))))
    (is (= :unknown-or-already-finished-continuation
           (reason #((:finish! f) (first @leases)))))
    (is (= 1 (:retained-continuations ((:summary! f)))))
    (is (= :finished (:status ((:finish! f) (second @leases)))))
    (is (= 0 (:retained-continuations ((:summary! f)))))
    (is (= :closed (:status ((:close! f)))))
    (is (= :closed (:status ((:close! other)))))))

(deftest retention-cap-is-enforced-without-early-release
  (let [f (issue!)
        captured (atom [])
        token (activate! f :test/bounded
                         (fn [_]
                           (let [tokens (vec (repeatedly 4096 #((:retain-current! f))))]
                             (reset! captured tokens)
                             {:status 200 :body
                              (str (reason #((:retain-current! f))))})))]
    (is (= ":too-many-registered-continuations" (:body ((:handler f) {}))))
    (is (= 4096 (:retained-continuations ((:summary! f)))))
    (is (= :registered-continuations-prevent-replacement
           (reason #((:activate! f)
                     (prepare! f :test/replacement (constantly response))))))
    (doseq [lease @captured] ((:finish! f) lease))
    (is (= 0 (:retained-continuations ((:summary! f)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest retirement-with-registered-work-times-out-fail-closed-and-retries
  (let [f (issue! {:drain-timeout-ms 35})
        lease (atom nil)
        token (activate! f :test/retire
                         (fn [_] (reset! lease ((:retain-current! f))) response))]
    (is (= response ((:handler f) {})))
    (is (= :registered-continuation-drain-timeout
           (reason #((:retire! f) token))))
    (is (= :draining (:phase ((:summary! f)))))
    (is (= 1 (:retained-continuations ((:summary! f)))))
    (is (denied? ((:handler f) {})))
    (is (= :previous-generation-still-draining
           (reason #(prepare! f :test/other (constantly response)))))
    (is (= :finished (:status ((:finish! f) @lease))))
    (is (= :retired (:status ((:retire! f) token))))
    (is (= :inactive (:phase ((:summary! f)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest closing-with-pending-work-times-out-but-can-complete-later
  (let [f (issue! {:drain-timeout-ms 35})
        lease (atom nil)
        token (activate! f :test/closing
                         (fn [_] (reset! lease ((:retain-current! f))) response))]
    (is (= response ((:handler f) {})))
    (is (= :registered-continuation-drain-timeout
           (reason #((:close! f)))))
    (is (= :closing (:phase ((:summary! f)))))
    (is (denied? ((:handler f) {})))
    (is (= :permanently-closed
           (reason #(prepare! f :test/reopen (constantly response)))))
    (is (= :finished (:status ((:finish! f) @lease))))
    (is (= :closed (:status ((:close! f)))))
    (is (= :closed (:phase ((:summary! f)))))
    (is (= :not-the-current-active-generation
           (reason #((:retire! f) token))))))

(deftest cross-thread-completion-unblocks-pending-retirement
  (let [f (issue! {:drain-timeout-ms 2000})
        lease (atom nil)
        entered (CountDownLatch. 1)
        token (activate! f :test/async
                         (fn [_] (reset! lease ((:retain-current! f)))
                           (down! entered) response))]
    (is (= response ((:handler f) {})))
    (is (reached? entered))
    (let [started (CountDownLatch. 1)
          retiring (daemon-task #(do (down! started) ((:retire! f) token)))]
      (is (reached? started))
      (is (= ::pending (deref (:result retiring) 35 ::pending)))
      (let [finishing (daemon-task #((:finish! f) @lease))]
        (is (= :finished (:status (completion finishing)))))
      (is (= :retired (:status (completion retiring))))
      (is (= :inactive (:phase ((:summary! f)))))
      (is (= 0 (:retained-continuations ((:summary! f)))))
      (is (= :closed (:status ((:close! f))))))))

(deftest cross-thread-completion-unblocks-pending-closure
  (let [f (issue! {:drain-timeout-ms 2000})
        lease (atom nil)
        token (activate! f :test/close-async
                         (fn [_] (reset! lease ((:retain-current! f))) response))]
    (is (= response ((:handler f) {})))
    (let [started (CountDownLatch. 1)
          closing (daemon-task #(do (down! started) ((:close! f))))]
      (is (reached? started))
      (is (= ::pending (deref (:result closing) 35 ::pending)))
      (let [finishing (daemon-task #((:finish! f) @lease))]
        (is (= :finished (:status (completion finishing)))))
      (is (= :closed (:status (completion closing))))
      (is (= :closed (:phase ((:summary! f)))))
      (is (denied? ((:handler f) {}))))))

(deftest synchronous-ring-call-holds-lease-until-handler-returns
  (let [f (issue! {:drain-timeout-ms 500})
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        token (activate! f :test/synchronous
                         (fn [_] (down! entered)
                           (if (.await release 2 TimeUnit/SECONDS)
                             response
                             (throw (ex-info "test release timed out" {})))))
        running (daemon-task #((:handler f) {}))]
    (try
      (is (reached? entered))
      (let [retiring (daemon-task #((:retire! f) token))]
        (is (= ::timed-out (deref (:result retiring) 30 ::timed-out)))
        (down! release)
        (is (= response (completion running)))
        (is (= :retired (:status (completion retiring))))
        (is (denied? ((:handler f) {}))))
      (finally (down! release) ((:close! f))))))

(deftest only-eager-ring-responses-pass-and-opaque-resources-are-closed
  (let [f (issue!)
        closed (atom 0)
        stream (proxy [ByteArrayInputStream] [(.getBytes "delayed" "UTF-8")]
                 (close [] (swap! closed inc) (proxy-super close)))
        replies [{:status 200 :body (lazy-seq ["late"])}
                 {:status 200 :body (fn [] "late")}
                 {:status 200 :body stream}
                 {:status 200 :body "ok" :session {:actor :x}}
                 {:status 200 :body "ok" :continuation (fn [] :later)}
                 {:status 200 :body "ok" :headers {"vary" [:not-eager]}}
                 {:status :ok :body "no"}
                 (with-meta {:status 200 :body "ok"} {:untrusted true})]
        index (atom -1)
        token (activate! f :test/eager
                         (fn [_] (nth replies (swap! index inc))))]
    (dotimes [_ (count replies)]
      (is (denied? ((:handler f) {}))))
    (is (= 1 @closed))
    (is (= 0 (:retained-continuations ((:summary! f)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest eager-byte-arrays-are-copied-before-return
  (let [f (issue!)
        owned (byte-array (map byte [10 20 30]))
        token (activate! f :test/bytes
                         (fn [_] {:status 200 :body owned}))
        reply ((:handler f) {})
        received (:body reply)]
    (is (= 200 (:status reply)))
    (is (not (identical? owned received)))
    (is (= [10 20 30] (mapv int received)))
    (aset-byte owned 0 (byte 99))
    (is (= [10 20 30] (mapv int received)))
    (is (= :closed (:status ((:close! f)))))))

(deftest admitted-nested-handler-preserves-correct-outer-lease
  (let [f (issue!)
        token-ids (atom [])
        depth (atom 0)
        t (activate! f :test/nested
                     (fn [req]
                       (swap! depth inc)
                       (try
                         (if (= 1 @depth)
                           (do
                             (swap! token-ids conj ((:retain-current! f)))
                             ((:handler f) {:nested true})
                             (swap! token-ids conj ((:retain-current! f)))
                             response)
                           (do
                             (swap! token-ids conj ((:retain-current! f)))
                             response))
                         (finally (swap! depth dec)))))]
    (is (= response ((:handler f) {})))
    (is (= 3 (count @token-ids)))
    (is (= 3 (:retained-continuations ((:summary! f)))))
    (doseq [id @token-ids] (is (= :finished (:status ((:finish! f) id)))))
    (is (= :closed (:status ((:close! f)))))))

(deftest interrupted-drain-remains-fail-closed-and-retryable
  (let [f (issue! {:drain-timeout-ms 500})
        lease (atom nil)
        token (activate! f :test/interrupted
                         (fn [_] (reset! lease ((:retain-current! f))) response))]
    (is (= response ((:handler f) {})))
    (let [retiring (daemon-task
                    (fn []
                      (.interrupt (Thread/currentThread))
                      {:reason (reason #((:retire! f) token))
                       :interrupt-preserved? (.isInterrupted (Thread/currentThread))}))
          outcome (completion retiring)]
      (is (= :registered-continuation-drain-interrupted (:reason outcome)))
      (is (true? (:interrupt-preserved? outcome)))
      (is (= :draining (:phase ((:summary! f)))))
      (is (denied? ((:handler f) {})))
      (is (= :finished (:status ((:finish! f) @lease))))
      (is (= :retired (:status ((:retire! f) token))))
      (is (= :closed (:status ((:close! f))))))))

(deftest rejecting-a-deferred-response-never-silently-releases-a-continuation
  (let [f (issue! {:drain-timeout-ms 30})
        lease (atom nil)
        token (activate! f :test/deferred
                         (fn [_]
                           (reset! lease ((:retain-current! f)))
                           {:status 200 :body (lazy-seq ["deferred"])}))]
    (is (denied? ((:handler f) {})))
    (is (= 1 (:retained-continuations ((:summary! f)))))
    (is (= :registered-continuation-drain-timeout
           (reason #((:retire! f) token))))
    (is (denied? ((:handler f) {})))
    (is (= :finished (:status ((:finish! f) @lease))))
    (is (= :retired (:status ((:retire! f) token))))
    (is (= :closed (:status ((:close! f)))))))

(deftest nested-guard-cannot-borrow-outer-request-ownership
  (let [f (issue!)
        observations (atom [])
        tokens (atom [])
        t (activate! f :test/nested-guard
                     (fn [req]
                       (if (:nested req)
                         (do (swap! tokens conj ((:retain-current! f))) response)
                         (do
                           (swap! tokens conj ((:retain-current! f)))
                           (doseq [mode [:denied :throws :allowed]]
                             (swap! observations conj
                                    (:status ((:handler f) {:nested mode}))))
                           (swap! tokens conj ((:retain-current! f)))
                           response)))
                     (fn [req]
                       (if-let [mode (:nested req)]
                         (do
                           (swap! observations conj
                                  (try
                                    (let [lease ((:retain-current! f))]
                                      ((:finish! f) lease)
                                      :borrowed-owner)
                                    (catch clojure.lang.ExceptionInfo e
                                      (:reason (ex-data e)))))
                           (case mode :denied false :throws (throw (Exception. "guard failed")) :allowed true))
                         true)))]
    (is (= response ((:handler f) {})))
    (is (= [:no-current-admitted-request 503
            :no-current-admitted-request 503
            :no-current-admitted-request 200] @observations))
    (is (= 3 (count @tokens)))
    (is (= 3 (:retained-continuations ((:summary! f)))))
    (doseq [lease @tokens] (is (= :finished (:status ((:finish! f) lease)))))
    (is (= :closed (:status ((:close! f)))))))
