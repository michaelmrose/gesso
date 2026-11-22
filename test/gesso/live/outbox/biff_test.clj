(ns gesso.live.outbox.biff-test
  "v2102: real Biff 2 module ordering, graceful drain fencing and
   embedded-XTDB/Live processing under native startup and teardown."
  (:require [clojure.test :refer [deftest is testing]]
            [com.biffweb.core :as biff]
            [gesso.live.core :as live]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture
             :refer [with-surfaced-application standard-operations render-context
                     rendered-operation-button]]
            [gesso.live.application-preflight :as application]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private source-id "gesso/biff-managed-test")
(def ^:dynamic *modules* [])

(defn- config
  ([] (config {}))
  ([extras]
   (merge {managed/source-id-key source-id
           managed/options-key {:worker-id "biff-worker" :poll-ms 20}
           managed/context-key {}}
          extras)))

(defn- invalid-reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e
                 (when (= :gesso.live.outbox/invalid-biff-lifecycle
                          (:error/type (ex-data e)))
                   (:reason (ex-data e))))))

(defn- live-system []
  (live/create {:rules [{:when-topic :request
                         :expand (fn [_ change]
                                   [(select-keys change [:topic :id])])}]
                :source-options {:coalesce-window-ms 0}
                :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}))

(defn- poll-until [f]
  (loop [n 0]
    (cond (f) true
          (>= n 250) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(deftest module-has-real-native-biff-contract-test
  (is (= :gesso.live.outbox/managed-worker (:biff.core/id managed/module)))
  (is (fn? (:biff.core/start managed/module)))
  (is (fn? (:biff.core/stop managed/module)))
  (is (identical? (:biff.core/start managed/module) managed/start-module!))
  (is (identical? (:biff.core/stop managed/module) managed/stop-module!)))

(deftest preflight-rejects-invalid-config-before-worker-creation-test
  (doseq [[opts expected]
          [[{managed/source-id-key "  "} :invalid-source-id]
           [{managed/options-key {}} :invalid-worker-options]
           [{managed/options-key {:worker-id "w" :poll-ms 1}} :invalid-worker-options]
           [{managed/options-key {:worker-id "w" :unknown :x}} :invalid-worker-options]
           [{managed/context-key nil} :invalid-rules-context]
           [{managed/stop-timeout-key -1} :invalid-stop-timeout]
           [{managed/handle-key :forged} :preexisting-handle]]]
    (is (= expected
           (invalid-reason #(managed/start-module! (config opts))))))
  (is (= :missing-or-closed-live-provider
         (invalid-reason #(managed/start-module! (config)))))
  (let [sys (live-system)]
    (try
      (is (= :missing-xtdb-provider
             (invalid-reason #(managed/start-module!
                               (assoc (config) :gesso.live/system sys)))))
      (finally (live/close! sys)))))

(deftest canonical-startup-checks-worker-identity-and-last-position-test
  (let [calls (atom [])
        start-order [application/application-handler-module-id managed/module-id]
        modules [application/application-handler-module managed/module]
        assembly ::opaque-assembly]
    (with-redefs [application/start-biff-application!
                  (fn [& args] (swap! calls conj args) :delegated)]
      (with-redefs [*modules* modules]
        (is (= :delegated (managed/start-biff-application!
                           assembly (config) #'*modules* start-order)))
        (let [[actual-assembly actual-system tracked-var actual-order] (first @calls)]
          (is (identical? assembly actual-assembly))
          (is (= (config) actual-system))
          (is (= start-order actual-order))
          (is (var? tracked-var))
          ;; Tracking may wrap provider/worker :start callbacks but must not
          ;; modify the host module Var or the canonical handler identity.
          (is (= (mapv :biff.core/id modules)
                 (mapv :biff.core/id @tracked-var)))
          (is (identical? (:biff.core/start application/application-handler-module)
                          (:biff.core/start (first @tracked-var))))))
      (doseq [[bad-modules order expected]
              [[[] start-order :invalid-worker-module-inventory]
               [[application/application-handler-module
                 (assoc managed/module :biff.core/start identity)]
                start-order :invalid-worker-module-inventory]
               [modules [managed/module-id application/application-handler-module-id]
                :worker-module-not-last]
               [modules [application/application-handler-module-id managed/module-id
                         managed/module-id] :worker-module-not-last]]]
        (with-redefs [*modules* bad-modules]
          (is (= expected (invalid-reason
                            #(managed/start-biff-application!
                              assembly (config) #'*modules* order))))))
      (is (= :invalid-worker-options
             (invalid-reason #(managed/start-biff-application!
                               assembly (config {managed/options-key
                                                 {:worker-id "w" :max-items 0}})
                               #'*modules* start-order))))
      (is (= 1 (count @calls)))
      (is (= :invalid-modules-var
             (invalid-reason #(managed/start-biff-application!
                                assembly (config) modules start-order)))))))

(deftest native-biff-stop-drains-before-provider-shutdown-test
  (with-open [node (xtn/start-node {})]
    (let [sys (live-system)
          events (atom [])
          xtdb-module {:biff.core/id :fixture/xtdb
                       :biff.core/start (fn [s] (swap! events conj :start-xtdb)
                                          (assoc s :xtdb/connectable node))
                       :biff.core/stop (fn [_] (swap! events conj :stop-xtdb))}
          live-module {:biff.core/id :fixture/live
                       :biff.core/start (fn [s] (swap! events conj :start-live)
                                          (assoc s :gesso.live/system sys))
                       :biff.core/stop (fn [_] (swap! events conj :stop-live)
                                         (live/close! sys))}]
      (binding [*modules* [xtdb-module live-module managed/module]]
        (let [started (biff/start (config) #'*modules*
                                  [:fixture/xtdb :fixture/live managed/module-id])
              handle (get started managed/handle-key)]
          (try
            (is (= [:start-xtdb :start-live] @events))
            (is (poll-until #(pos? (:cycles (lifecycle/status handle)))))
            (is (= source-id (:source-id (lifecycle/status handle))))
            (finally (biff/stop started)))
          (is (= [:start-xtdb :start-live :stop-live :stop-xtdb] @events))
          (is (= :stopped (:status (lifecycle/status handle)))))))))

(deftest native-biff-stop-timeout-prevents-dependency-close-test
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        events (atom [])
        stop-calls (atom 0)]
    (with-redefs [lifecycle/start!
                  (fn [& _] (Object.))
                  lifecycle/retire!
                  (fn [_ _]
                    (if (= 1 (swap! stop-calls inc))
                      {:outcome :timed-out}
                      {:outcome :drained}))]
      (let [providers {:biff.core/id :fixture/providers
                       :biff.core/start
                       (fn [s] (assoc s :xtdb/connectable :fake-node
                                      :gesso.live/system
                                      {:source :fake :closed? (atom false)}))
                       :biff.core/stop (fn [_] (swap! events conj :providers-closed))}]
        (binding [*modules* [providers managed/module]]
          (let [started (biff/start (config {managed/stop-timeout-key 0})
                                    #'*modules* [:fixture/providers managed/module-id])]
            (is (= :drain-timed-out (invalid-reason #(biff/stop started))))
            (is (empty? @events))
            (biff/stop started)
            (is (= [:providers-closed] @events))
            (is (= 2 @stop-calls))))))))

(deftest native-biff-worker-processes-outbox-using-supplied-providers-test
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          plan (outbox/prepare!
                {:intent-ids [id] :source-id source-id
                 :operation :request/claim :purpose :gesso.live/refresh}
                [{:topic :request :id "biff-request"}])
          _ (xt/execute-tx node (conj (vec (:guard-ops plan)) (:tx-op plan)))
          sys (live-system)
          provider {:biff.core/id :fixture/providers
                    :biff.core/start (fn [s] (assoc s :xtdb/connectable node
                                                   :gesso.live/system sys))
                    :biff.core/stop (fn [_] (live/close! sys))}]
      (binding [*modules* [provider managed/module]]
        (let [started (biff/start (config) #'*modules*
                                  [:fixture/providers managed/module-id])
              handle (get started managed/handle-key)]
          (try
            (is (poll-until #(= :dispatch-accepted
                                (:gesso.live.outbox/status
                                  (first (xt/q node
                                               ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id]))))))
            ;; XTDB acknowledgement may become visible just before the
            ;; worker publishes its in-memory completed-cycle counters.
            (is (poll-until #(= 1 (:total-dispatch-accepted
                                   (lifecycle/status handle)))))
            (finally (biff/stop started)))
          (is (= :stopped (:status (lifecycle/status handle)))))))))

(deftest canonical-gesso-preflight-starts-native-managed-biff-worker-test
  ;; Unlike the identity-only delegation test, this uses a real current
  ;; ApplicationAssembly and the public Gesso canonical Biff startup path.
  (let [ops (standard-operations)
        render (render-context ops)]
    (with-surfaced-application
      {:request-board
       [:main (rendered-operation-button render :request/claim
                                         "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (with-open [node (xtn/start-node {})]
          (let [sys (live-system)
                events (atom [])
                provider {:biff.core/id :fixture/providers
                          :biff.core/start (fn [s] (assoc s :xtdb/connectable node
                                                        :gesso.live/system sys))
                          :biff.core/stop (fn [_]
                                            (swap! events conj :stop-providers)
                                            (live/close! sys))}
                handler (fn [_]
                          (g/html-response
                            [:main (rendered-operation-button
                                    render :request/claim "/operations/request/claim")]))]
            (binding [*modules* [application/application-handler-module
                                 provider managed/module]]
              (let [started (managed/start-biff-application!
                              assembly
                              (assoc (config) :biff.ring/handler handler)
                              #'*modules*
                              [application/application-handler-module-id
                               :fixture/providers managed/module-id])
                    handle (get started managed/handle-key)]
                (try
                  (is (fn? (:biff.ring/handler started)))
                  (is (not (identical? handler (:biff.ring/handler started))))
                  (is (= 200 (:status ((:biff.ring/handler started) {:uri "/"}))))
                  (is (poll-until #(pos? (:cycles (lifecycle/status handle)))))
                  (finally (biff/stop started)))
                (is (= [:stop-providers] @events))
                (is (= :stopped (:status (lifecycle/status handle))))))))))))


(defn- thrown-startup
  [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error error)))

(deftest checked-startup-undoes-completed-biff-modules-in-reverse-order-test
  ;; Fail after two physical resource-owning modules have successfully
  ;; returned, but BEFORE the managed worker starts. Native biff/start does
  ;; not unwind those modules. The checked entry must close Live then XTDB.
  (let [ops (standard-operations)
        render (render-context ops)]
    (with-surfaced-application
      {:request-board
       [:main (rendered-operation-button render :request/claim
                                         "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [events (atom [])
              resources (atom {})
              boom (ex-info "provider startup exploded" {:test :last-module})
              provider-node {:biff.core/id :fixture/xtdb
                             :biff.core/start
                             (fn [system]
                               (let [node (xtn/start-node {})]
                                 (swap! resources assoc :node node)
                                 (swap! events conj :start-xtdb)
                                 (assoc system :xtdb/connectable node)))
                             :biff.core/stop
                             (fn [system]
                               (swap! events conj :stop-xtdb)
                               (.close ^java.lang.AutoCloseable (:xtdb/connectable system)))}
              provider-live {:biff.core/id :fixture/live
                             :biff.core/start
                             (fn [system]
                               (let [source (live-system)]
                                 (swap! resources assoc :live source)
                                 (swap! events conj :start-live)
                                 (assoc system :gesso.live/system source)))
                             :biff.core/stop
                             (fn [system]
                               (swap! events conj :stop-live)
                               (live/close! (:gesso.live/system system)))}
              doomed {:biff.core/id :fixture/throw
                      :biff.core/start (fn [_]
                                         (swap! events conj :start-throw)
                                         (throw boom))}]
          (binding [*modules* [application/application-handler-module
                               provider-node provider-live doomed managed/module]]
            (let [error (thrown-startup
                         #(managed/start-biff-application!
                           assembly
                           (assoc (config)
                                  :biff.ring/handler
                                  (fn [_] (g/html-response
                                            [:main (rendered-operation-button
                                                    render :request/claim
                                                    "/operations/request/claim")])))
                           #'*modules*
                           [application/application-handler-module-id
                            :fixture/xtdb :fixture/live :fixture/throw
                            managed/module-id]))]
              (is (= :gesso.live.outbox/checked-startup-failed
                     (:error/type (ex-data error))))
              (is (= :drained (:rollback/status (ex-data error))))
              (is (identical? boom (.getCause error)))
              (is (nil? (:cleanup/handle (ex-data error))))
              (is (= [:start-xtdb :start-live :start-throw
                      :stop-live :stop-xtdb]
                     @events))
              (is (live/closed? (:live @resources)))
              (is (some? (:node @resources))))))))))

(deftest checked-startup-blocked-unwind-retains-retryable-ownership-test
  ;; When a provider's stop fails, it must not close the earlier dependency.
  ;; A retry handle retaining the completed system is the only authority to
  ;; resume teardown. Repeated retry after success must not double-close.
  (let [events (atom [])
        stop-attempts (atom 0)
        boom (ex-info "failed provider start" {:test :startup})
        dependency {:biff.core/id :fixture/dependency
                    :biff.core/start (fn [s] (swap! events conj :start-dependency)
                                       (assoc s :fixture/dependency :open))
                    :biff.core/stop (fn [_] (swap! events conj :stop-dependency))}
        upper {:biff.core/id :fixture/upper
               :biff.core/start (fn [s] (swap! events conj :start-upper)
                                  (assoc s :fixture/upper :open))
               :biff.core/stop
               (fn [_]
                 (swap! events conj :stop-upper)
                 (when (= 1 (swap! stop-attempts inc))
                   (throw (ex-info "drain timed out" {:test :timeout}))))}
        doomed {:biff.core/id :fixture/doomed
                :biff.core/start (fn [_] (throw boom))}]
    (binding [*modules* [application/application-handler-module
                         dependency upper doomed managed/module]]
      ;; Stub ONLY the canonical application startup validation. It still
      ;; calls real native Biff.start on the tracked modules Var, so module
      ;; start/stop semantics are physical Biff behavior, not a mocked loop.
      (with-redefs [application/start-biff-application!
                    (fn [_assembly initial modules order]
                      ;; Bypass only the canonical handler's assembly
                      ;; recognition; keep actual Biff provider lifecycle.
                      (let [providers (clojure.lang.Var/create
                                       (vec (rest @modules)))]
                        (biff/start initial providers (vec (rest order)))))]
        (let [error (thrown-startup
                     #(managed/start-biff-application!
                       ::assembly (config) #'*modules*
                       [application/application-handler-module-id
                        :fixture/dependency :fixture/upper :fixture/doomed
                        managed/module-id]))
              data (ex-data error)
              handle (:cleanup/handle data)]
          (is (= :blocked (:rollback/status data)))
          (is (= :fixture/upper (:rollback/blocked-module-id data)))
          (is (= [:fixture/dependency :fixture/upper]
                 (:rollback/remaining-module-ids data)))
          (is (identical? boom (.getCause error)))
          (is (= 1 (count (.getSuppressed error))))
          (is (= "drain timed out" (.getMessage (first (.getSuppressed error)))))
          (is (= [:start-dependency :start-upper :stop-upper] @events))
          (is (= "#<GessoOutboxStartupCleanup>" (str handle)))
          (is (= :invalid-startup-cleanup-handle
                 (invalid-reason #(managed/retry-startup-cleanup! nil))))
          (is (= :drained (:status (managed/retry-startup-cleanup! handle))))
          (is (= [:start-dependency :start-upper :stop-upper
                  :stop-upper :stop-dependency] @events))
          (is (= :drained (:status (managed/retry-startup-cleanup! handle))))
          (is (= 2 @stop-attempts))
          (is (= 5 (count @events))))))))

(deftest checked-startup-worker-start-failure-rolls-back-earlier-providers-test
  (let [events (atom [])
        provider {:biff.core/id :fixture/provider
                  :biff.core/start
                  (fn [sys]
                    (swap! events conj :start-provider)
                    (assoc sys :xtdb/connectable :fake
                               :gesso.live/system {:source :fake
                                                   :closed? (atom false)}))
                  :biff.core/stop (fn [_] (swap! events conj :stop-provider))}
        failure (ex-info "thread could not start" {:test :worker})]
    (binding [*modules* [application/application-handler-module provider
                         managed/module]]
      (with-redefs [application/start-biff-application!
                    (fn [_assembly initial modules order]
                      ;; Bypass only the canonical handler's assembly
                      ;; recognition; keep actual Biff provider lifecycle.
                      (let [providers (clojure.lang.Var/create
                                       (vec (rest @modules)))]
                        (biff/start initial providers (vec (rest order)))))
                    lifecycle/start! (fn [& _] (throw failure))]
        (let [e (thrown-startup
                 #(managed/start-biff-application!
                   ::assembly (config) #'*modules*
                   [application/application-handler-module-id
                    :fixture/provider managed/module-id]))]
          (is (= :drained (:rollback/status (ex-data e))))
          (is (identical? failure (.getCause e)))
          (is (= [:start-provider :stop-provider] @events)))))))

(deftest checked-startup-success-preserves-native-biff-stop-chain-test
  (let [events (atom [])
        provider {:biff.core/id :fixture/provider
                  :biff.core/start
                  (fn [sys]
                    (swap! events conj :start-provider)
                    (assoc sys :xtdb/connectable :fake
                               :gesso.live/system {:source :fake
                                                   :closed? (atom false)}))
                  :biff.core/stop (fn [_] (swap! events conj :stop-provider))}]
    (binding [*modules* [application/application-handler-module provider
                         managed/module]]
      (with-redefs [application/start-biff-application!
                    (fn [_assembly initial modules order]
                      ;; Bypass only the canonical handler's assembly
                      ;; recognition; keep actual Biff provider lifecycle.
                      (let [providers (clojure.lang.Var/create
                                       (vec (rest @modules)))]
                        (biff/start initial providers (vec (rest order)))))
                    lifecycle/start! (fn [& _] :fake-handle)
                    lifecycle/retire! (fn [& _]
                                      (swap! events conj :stop-worker)
                                      {:outcome :drained})]
        (let [started (managed/start-biff-application!
                       ::assembly (config) #'*modules*
                       [application/application-handler-module-id
                        :fixture/provider managed/module-id])]
          (is (some? (get started managed/handle-key)))
          (is (= [:start-provider] @events))
          (biff/stop started)
          (is (= [:start-provider :stop-worker :stop-provider] @events)))))))

(defn- start-checked-fixture!
  "Run real native Biff startup with the tracked modules Var. Only the
   canonical application-assembly admission is replaced by a test fixture."
  [modules]
  (binding [*modules* (vec (concat [application/application-handler-module]
                                  modules [managed/module]))]
    (with-redefs [application/start-biff-application!
                  (fn [_ initial modules-var order]
                    (biff/start initial
                                (clojure.lang.Var/create (vec (rest @modules-var)))
                                (vec (rest order))))]
      (managed/start-biff-application!
       ::fixture (config) #'*modules*
       (vec (concat [application/application-handler-module-id]
                    (map :biff.core/id modules)
                    [managed/module-id]))))))

(deftest resource-acquisition-requires-live-checked-startup-scope-test
  (let [calls (atom 0)]
    (is (= :no-active-startup-ownership
           (invalid-reason
            #(managed/acquire-startup-resource!
              :fixture/resource
              (fn [] (swap! calls inc) :resource)
              (fn [_] (swap! calls inc))))))
    (is (zero? @calls))))

(deftest partial-startup-owned-resources-rollback-before-dependencies-test
  (let [events (atom [])
        provider {:biff.core/id :fixture/dependency
                  :biff.core/start (fn [s]
                                     (swap! events conj :start-dependency)
                                     (assoc s :fixture/dependency :open))
                  :biff.core/stop (fn [_] (swap! events conj :stop-dependency))}
        fault {:biff.core/id :fixture/partially-started
               :biff.core/start
               (fn [_]
                 (let [a (managed/acquire-startup-resource!
                          :fixture/a
                          (fn [] (swap! events conj :acquire-a) :a)
                          (fn [_] (swap! events conj :close-a)))
                       b (managed/acquire-startup-resource!
                          :fixture/b
                          (fn [] (swap! events conj :acquire-b) :b)
                          (fn [_] (swap! events conj :close-b)))]
                   (is (= [:a :b] [a b]))
                   (throw (ex-info "failure after acquisition" {:test :partial}))))
               :biff.core/stop (fn [_] (swap! events conj :should-not-stop-module))}
        error (thrown-startup #(start-checked-fixture! [provider fault]))]
    (is (= :gesso.live.outbox/checked-startup-failed
           (:error/type (ex-data error))))
    (is (= :drained (:rollback/status (ex-data error))))
    (is (= :partial (:test (ex-data (.getCause error)))))
    (is (= [:start-dependency :acquire-a :acquire-b
            :close-b :close-a :stop-dependency]
           @events))))

(deftest partial-physical-xtdb-allocation-is-closed-on-start-error-test
  (let [events (atom [])
        node-atom (atom nil)
        provider {:biff.core/id :fixture/physical-node
                  :biff.core/start
                  (fn [_]
                    (managed/acquire-startup-resource!
                     :fixture/xtdb
                     (fn [] (let [node (xtn/start-node {})]
                              (reset! node-atom node)
                              (swap! events conj :node-open)
                              node))
                     (fn [node]
                       (.close ^java.lang.AutoCloseable node)
                       (swap! events conj :node-closed)))
                    (throw (ex-info "physical start failed" {})))
                  :biff.core/stop
                  (fn [_] (swap! events conj :should-not-stop-module))}
        error (thrown-startup #(start-checked-fixture! [provider]))]
    (is (= :drained (:rollback/status (ex-data error))))
    (is (some? @node-atom))
    (is (= [:node-open :node-closed] @events))))

(deftest blocked-partial-resource-drain-fences-earlier-dependency-test
  (let [events (atom [])
        stop-attempts (atom 0)
        dependency {:biff.core/id :fixture/dependency
                    :biff.core/start (fn [s]
                                       (swap! events conj :dependency-start)
                                       s)
                    :biff.core/stop (fn [_] (swap! events conj :dependency-stop))}
        fault {:biff.core/id :fixture/fault
               :biff.core/start
               (fn [_]
                 (managed/acquire-startup-resource!
                  :fixture/first
                  (fn [] (swap! events conj :first-start) :first)
                  (fn [_] (swap! events conj :first-stop)))
                 (managed/acquire-startup-resource!
                  :fixture/second
                  (fn [] (swap! events conj :second-start) :second)
                  (fn [_]
                    (swap! events conj :second-stop)
                    (when (= 1 (swap! stop-attempts inc))
                      (throw (ex-info "resource still busy" {})))))
                 (throw (ex-info "module failed" {})))
               :biff.core/stop (fn [_] (swap! events conj :should-not-stop))}
        error (thrown-startup #(start-checked-fixture! [dependency fault]))
        data (ex-data error)
        handle (:cleanup/handle data)]
    (is (= :blocked (:rollback/status data)))
    (is (= :fixture/fault (:rollback/blocked-module-id data)))
    (is (= :fixture/second (:rollback/blocked-resource-id data)))
    (is (some? handle))
    (is (= [:dependency-start :first-start :second-start :second-stop]
           @events))
    (is (= :drained (:status (managed/retry-startup-cleanup! handle))))
    (is (= [:dependency-start :first-start :second-start :second-stop
            :second-stop :first-stop :dependency-stop]
           @events))
    (is (= :drained (:status (managed/retry-startup-cleanup! handle))))
    (is (= 2 @stop-attempts))))

(deftest returned-module-transfers-partial-ownership-to-native-biff-stop-test
  (let [events (atom [])
        provider {:biff.core/id :fixture/owned-provider
                  :biff.core/start
                  (fn [s]
                    (let [resource (managed/acquire-startup-resource!
                                    :fixture/resource
                                    (fn [] (swap! events conj :acquire) :resource)
                                    (fn [_] (swap! events conj :partial-rollback)))]
                      (assoc s :fixture/owned-resource resource
                               :xtdb/connectable :fake
                               :gesso.live/system {:source :fake
                                                   :closed? (atom false)})))
                  :biff.core/stop
                  (fn [s]
                    (is (= :resource (:fixture/owned-resource s)))
                    (swap! events conj :normal-stop))}]
    (with-redefs [lifecycle/start! (fn [& _] :worker-handle)
                  lifecycle/retire! (fn [& _] {:outcome :drained})]
      (let [started (start-checked-fixture! [provider])]
        (is (= :resource (:fixture/owned-resource started)))
        (is (= [:acquire] @events))
        (biff/stop started)
        (is (= [:acquire :normal-stop] @events))))))

(deftest module-with-owned-resource-must-have-stop-before-ownership-transfer-test
  (let [events (atom [])
        module {:biff.core/id :fixture/no-stop
                :biff.core/start
                (fn [s]
                  (managed/acquire-startup-resource!
                   :fixture/resource
                   (fn [] (swap! events conj :acquire) :resource)
                   (fn [_] (swap! events conj :partial-rollback)))
                  s)}
        error (thrown-startup #(start-checked-fixture! [module]))]
    (is (= :drained (:rollback/status (ex-data error))))
    (is (= :missing-owned-module-stop
           (:reason (ex-data (.getCause error)))))
    (is (= [:acquire :partial-rollback] @events))))

(deftest duplicate-resource-registration-compensates-new-acquisition-test
  (let [events (atom [])
        module {:biff.core/id :fixture/duplicate
                :biff.core/start
                (fn [_]
                  (managed/acquire-startup-resource!
                   :fixture/resource
                   (fn [] (swap! events conj :first-acquire) :first)
                   (fn [_] (swap! events conj :first-close)))
                  (managed/acquire-startup-resource!
                   :fixture/resource
                   (fn [] (swap! events conj :second-acquire) :second)
                   (fn [_] (swap! events conj :second-close))))
                :biff.core/stop (fn [_] (swap! events conj :unexpected-stop))}
        error (thrown-startup #(start-checked-fixture! [module]))]
    (is (= :duplicate-owned-resource
           (:reason (ex-data (.getCause error)))))
    (is (= :drained (:rollback/status (ex-data error))))
    (is (= [:first-acquire :second-acquire :second-close :first-close]
           @events))))

(deftest init-admission-rejects-unclassified-functions-before-biff-runs-test
  (let [calls (atom 0)
        provider {:biff.core/id :fixture/unadmitted
                  :biff.core/init (fn [_] (swap! calls inc) {})
                  :biff.core/start (fn [s] (swap! calls inc) s)}
        error (thrown-startup #(start-checked-fixture! [provider]))]
    (is (= :unadmitted-init (:reason (ex-data error))))
    (is (zero? @calls))
    (is (= :tracked-init-must-be-callable
           (invalid-reason
            #(start-checked-fixture!
              [(assoc provider :biff.core/init {}
                               managed/init-policy-key :tracked)]))))
    (is (zero? @calls))))

(deftest explicit-declared-pure-init-stays-compatible-with-biff-merge-test
  (let [events (atom [])
        module {:biff.core/id :fixture/pure
                managed/init-policy-key :declared-pure
                :biff.core/init
                (fn [_]
                  (swap! events conj :init)
                  {:fixture/pure-setting 42})
                :biff.core/start
                (fn [system]
                  (is (= 42 (:fixture/pure-setting system)))
                  (swap! events conj :start)
                  (assoc system :xtdb/connectable :fake
                                :gesso.live/system
                                {:source :fake :closed? (atom false)}))
                :biff.core/stop (fn [_] (swap! events conj :stop))}]
    (with-redefs [lifecycle/start! (fn [& _] :worker-handle)
                  lifecycle/retire! (fn [& _] {:outcome :drained})]
      (let [running (start-checked-fixture! [module])]
        (is (= 42 (:fixture/pure-setting running)))
        (is (= [:init :start] @events))
        (biff/stop running)
        (is (= [:init :start :stop] @events))))))

(deftest pure-init-cannot-use-owned-resource-helper-test
  (let [calls (atom 0)
        module {:biff.core/id :fixture/pure
                managed/init-policy-key :declared-pure
                :biff.core/init
                (fn [_]
                  (managed/acquire-init-resource!
                   :fixture/forbidden
                   (fn [] (swap! calls inc) :resource)
                   (fn [_] (swap! calls inc)))
                  {})
                :biff.core/start identity}
        error (thrown-startup #(start-checked-fixture! [module]))]
    (is (= :gesso.live.outbox/checked-startup-failed
           (:error/type (ex-data error))))
    (is (= :no-active-startup-ownership
           (:reason (ex-data (.getCause error)))))
    (is (= :drained (:rollback/status (ex-data error))))
    (is (zero? @calls))))

(deftest tracked-init-partial-resource-rollback-precedes-module-start-test
  (let [events (atom [])
        init {:biff.core/id :fixture/init
              managed/init-policy-key :tracked
              :biff.core/init (fn [_]
                                (managed/acquire-init-resource!
                                 :fixture/one
                                 (fn [] (swap! events conj :acquire-one) :one)
                                 (fn [_] (swap! events conj :close-one)))
                                (managed/acquire-init-resource!
                                 :fixture/two
                                 (fn [] (swap! events conj :acquire-two) :two)
                                 (fn [_] (swap! events conj :close-two)))
                                (throw (ex-info "init exploded" {:fixture/error true})))
              :biff.core/start (fn [s] (swap! events conj :should-not-start) s)}
        failure (thrown-startup #(start-checked-fixture! [init]))]
    (is (= :drained (:rollback/status (ex-data failure))))
    (is (true? (:fixture/error (ex-data (.getCause failure)))))
    (is (= [:acquire-one :acquire-two :close-two :close-one] @events))))

(deftest tracked-init-allocated-real-xtdb-is-closed-on-later-init-failure-test
  (let [node-ref (atom nil)
        closed (atom false)
        module {:biff.core/id :fixture/init-xtdb
                managed/init-policy-key :tracked
                :biff.core/init
                (fn [_]
                  (managed/acquire-init-resource!
                   :fixture/xtdb
                   (fn [] (let [node (xtn/start-node {})]
                            (reset! node-ref node)
                            node))
                   (fn [node]
                     (.close ^java.lang.AutoCloseable node)
                     (reset! closed true)))
                  (throw (ex-info "init failed with node allocated" {})))
                :biff.core/start identity}
        failure (thrown-startup #(start-checked-fixture! [module]))]
    (is (= :drained (:rollback/status (ex-data failure))))
    (is (some? @node-ref))
    (is (true? @closed))))

(deftest init-resource-rolls-back-after-completed-started-module-test
  (let [events (atom [])
        initialized {:biff.core/id :fixture/init
                     managed/init-policy-key :tracked
                     :biff.core/init (fn [_]
                                       (managed/acquire-init-resource!
                                        :fixture/init-resource
                                        (fn [] (swap! events conj :init-acquire) :resource)
                                        (fn [_] (swap! events conj :init-close)))
                                       {:fixture/initialized true})
                     :biff.core/start (fn [s] (swap! events conj :init-start) s)
                     :biff.core/stop (fn [_] (swap! events conj :init-stop))}
        started {:biff.core/id :fixture/started
                 :biff.core/start (fn [s] (swap! events conj :started-start) s)
                 :biff.core/stop (fn [_] (swap! events conj :started-stop))}
        doomed {:biff.core/id :fixture/doomed
                :biff.core/start (fn [_] (throw (ex-info "start exploded" {})))}
        failure (thrown-startup #(start-checked-fixture!
                                  [initialized started doomed]))]
    (is (= :drained (:rollback/status (ex-data failure))))
    (is (= [:init-acquire :init-start :started-start
            :started-stop :init-stop :init-close]
           @events))))

(deftest init-resources-close-after-all-native-module-stops-test
  (let [events (atom [])
        init {:biff.core/id :fixture/init
              managed/init-policy-key :tracked
              :biff.core/init
              (fn [_]
                (managed/acquire-init-resource!
                 :fixture/one
                 (fn [] (swap! events conj :init-one) :one)
                 (fn [_] (swap! events conj :close-one)))
                (managed/acquire-init-resource!
                 :fixture/two
                 (fn [] (swap! events conj :init-two) :two)
                 (fn [_] (swap! events conj :close-two)))
                {:fixture/init-available true})
              :biff.core/start (fn [s] (swap! events conj :start-init) s)
              :biff.core/stop (fn [_] (swap! events conj :stop-init))}
        provider {:biff.core/id :fixture/provider
                  :biff.core/start (fn [s]
                                     (is (true? (:fixture/init-available s)))
                                     (swap! events conj :start-provider)
                                     (assoc s :xtdb/connectable :fake
                                              :gesso.live/system
                                              {:source :fake :closed? (atom false)}))
                  :biff.core/stop (fn [_] (swap! events conj :stop-provider))}]
    (with-redefs [lifecycle/start! (fn [& _] :worker)
                  lifecycle/retire! (fn [& _]
                                    (swap! events conj :stop-worker)
                                    {:outcome :drained})]
      (let [running (start-checked-fixture! [init provider])]
        (is (= [:init-one :init-two :start-init :start-provider] @events))
        (biff/stop running)
        (is (= [:init-one :init-two :start-init :start-provider
                :stop-worker :stop-provider :stop-init :close-two :close-one]
               @events))
        (biff/stop running)
        (is (= 9 (count @events)))))))

(deftest blocked-init-rollback-is-retryable-and-fences-previous-init-resources-test
  (let [events (atom [])
        attempts (atom 0)
        init {:biff.core/id :fixture/init
              managed/init-policy-key :tracked
              :biff.core/init (fn [_]
                                (managed/acquire-init-resource!
                                 :fixture/first
                                 (fn [] (swap! events conj :acquire-first) :first)
                                 (fn [_] (swap! events conj :close-first)))
                                (managed/acquire-init-resource!
                                 :fixture/second
                                 (fn [] (swap! events conj :acquire-second) :second)
                                 (fn [_]
                                   (swap! events conj :close-second)
                                   (when (= 1 (swap! attempts inc))
                                     (throw (ex-info "initial cleanup refused" {})))))
                                (throw (ex-info "init error" {})))
              :biff.core/start identity}
        error (thrown-startup #(start-checked-fixture! [init]))
        data (ex-data error)
        handle (:cleanup/handle data)]
    (is (= :blocked (:rollback/status data)))
    (is (= :fixture/init (:rollback/blocked-module-id data)))
    (is (= :fixture/second (:rollback/blocked-resource-id data)))
    (is (some? handle))
    (is (= [:acquire-first :acquire-second :close-second] @events))
    (is (= :drained (:status (managed/retry-startup-cleanup! handle))))
    (is (= [:acquire-first :acquire-second :close-second
            :close-second :close-first] @events))
    (is (= :drained (:status (managed/retry-startup-cleanup! handle))))))

(deftest blocked-normal-init-cleanup-can-retry-stop-without-double-stopping-providers-test
  (let [events (atom [])
        attempts (atom 0)
        init {:biff.core/id :fixture/init
              managed/init-policy-key :tracked
              :biff.core/init (fn [_]
                                (managed/acquire-init-resource!
                                 :fixture/one
                                 (fn [] (swap! events conj :init-acquire) :one)
                                 (fn [_]
                                   (swap! events conj :init-close)
                                   (when (= 1 (swap! attempts inc))
                                     (throw (ex-info "cleanup unavailable" {})))))
                                {})
              :biff.core/start identity}
        provider {:biff.core/id :fixture/provider
                  :biff.core/start (fn [s]
                                     (swap! events conj :start-provider)
                                     (assoc s :xtdb/connectable :fake
                                              :gesso.live/system
                                              {:source :fake :closed? (atom false)}))
                  :biff.core/stop (fn [_] (swap! events conj :stop-provider))}]
    (with-redefs [lifecycle/start! (fn [& _] :worker)
                  lifecycle/retire! (fn [& _]
                                    (swap! events conj :stop-worker)
                                    {:outcome :drained})]
      (let [running (start-checked-fixture! [init provider])
            first-try (thrown-startup #(biff/stop running))]
        (is (= :gesso.live.outbox/init-cleanup-blocked
               (:error/type (ex-data first-try))))
        (is (= :fixture/one (:blocked-resource-id (ex-data first-try))))
        (is (= [:init-acquire :start-provider :stop-worker
                :stop-provider :init-close] @events))
        (biff/stop running)
        (is (= [:init-acquire :start-provider :stop-worker
                :stop-provider :init-close :init-close] @events))
        (is (= 2 @attempts))
        (biff/stop running)
        (is (= 6 (count @events)))))))

(deftest native-provider-stop-failure-does-not-close-init-resource-test
  (let [events (atom [])
        attempts (atom 0)
        init {:biff.core/id :fixture/init
              managed/init-policy-key :tracked
              :biff.core/init (fn [_]
                                (managed/acquire-init-resource!
                                 :fixture/resource
                                 (fn [] (swap! events conj :init-acquire) :x)
                                 (fn [_] (swap! events conj :init-close)))
                                {})
              :biff.core/start identity}
        provider {:biff.core/id :fixture/provider
                  :biff.core/start (fn [s]
                                     (assoc s :xtdb/connectable :fake
                                              :gesso.live/system
                                              {:source :fake :closed? (atom false)}))
                  :biff.core/stop (fn [_]
                                    (swap! events conj :stop-provider)
                                    (when (= 1 (swap! attempts inc))
                                      (throw (ex-info "provider busy" {}))))}]
    (with-redefs [lifecycle/start! (fn [& _] :worker)
                  lifecycle/retire! (fn [& _] {:outcome :drained})]
      (let [running (start-checked-fixture! [init provider])]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"provider busy"
                              (biff/stop running)))
        (is (= [:init-acquire :stop-provider] @events))
        (biff/stop running)
        (is (= [:init-acquire :stop-provider :stop-provider :init-close]
               @events))))))

(deftest duplicate-biff-init-result-keys-unwind-tracked-resources-test
  ;; Biff evaluates every init callback before it detects conflicting keys.
  ;; A failed safe-merge is not a failed :start; no start-module callback runs.
  (let [events (atom [])
        a {:biff.core/id :fixture/init-a
           managed/init-policy-key :tracked
           :biff.core/init (fn [_]
                             (managed/acquire-init-resource!
                              :fixture/a
                              (fn [] (swap! events conj :init-a) :a)
                              (fn [_] (swap! events conj :close-a)))
                             {:fixture/shared 1})
           :biff.core/start (fn [s] (swap! events conj :should-not-start-a) s)}
        b {:biff.core/id :fixture/init-b
           managed/init-policy-key :tracked
           :biff.core/init (fn [_]
                             (managed/acquire-init-resource!
                              :fixture/b
                              (fn [] (swap! events conj :init-b) :b)
                              (fn [_] (swap! events conj :close-b)))
                             {:fixture/shared 2})
           :biff.core/start (fn [s] (swap! events conj :should-not-start-b) s)}
        failure (thrown-startup #(start-checked-fixture! [a b]))]
    (is (= :gesso.live.outbox/checked-startup-failed
           (:error/type (ex-data failure))))
    (is (= :drained (:rollback/status (ex-data failure))))
    (is (instance? AssertionError (.getCause failure)))
    (is (= [:init-a :init-b :close-b :close-a] @events))))

(deftest unadmitted-later-init-is-rejected-before-earlier-init-executes-test
  (let [events (atom [])
        first-module {:biff.core/id :fixture/earlier-init
                      managed/init-policy-key :tracked
                      :biff.core/init
                      (fn [_]
                        (swap! events conj :init-executed)
                        {:fixture/value 1})
                      :biff.core/start identity}
        second-module {:biff.core/id :fixture/unadmitted-later
                       :biff.core/init (fn [_]
                                         (swap! events conj :unsafe-init-executed)
                                         {})
                       :biff.core/start identity}
        failure (thrown-startup #(start-checked-fixture!
                                  [first-module second-module]))]
    (is (= :unadmitted-init (:reason (ex-data failure))))
    (is (= :fixture/unadmitted-later (:module-id (ex-data failure))))
    (is (empty? @events))))

(deftest interrupted-checked-startup-restores-caller-cancellation-test
  ;; Native Biff already failed a provider :start. Checked startup must unwind
  ;; the completed provider, retain the exact cause and restore the JVM signal
  ;; after rollback, which must still be allowed to attempt physical cleanup.
  (let [events (atom [])
        interruption (InterruptedException. "provider start interrupted")
        provider {:biff.core/id :fixture/dependency
                  :biff.core/start (fn [system]
                                     (swap! events conj :start-dependency)
                                     system)
                  :biff.core/stop (fn [_]
                                    (swap! events conj :stop-dependency))}
        doomed {:biff.core/id :fixture/doomed
                :biff.core/start (fn [_]
                                   (swap! events conj :start-doomed)
                                   (throw interruption))}]
    (try
      (Thread/interrupted)
      (let [error (try (start-checked-fixture! [provider doomed])
                       (catch Throwable failure failure))
            interrupted? (.isInterrupted (Thread/currentThread))]
        (Thread/interrupted)
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (= :gesso.live.outbox/checked-startup-failed
               (:error/type (ex-data error))))
        (is (= :drained (:rollback/status (ex-data error))))
        (is (identical? interruption (.getCause ^Throwable error)))
        (is (= [:start-dependency :start-doomed :stop-dependency] @events)
            "Rollback must release a started dependency after interruption")
        (is interrupted? "Checked startup must retain JVM interruption"))
      (finally
        (Thread/interrupted)))))

(deftest interrupted-checked-rollback-stop-retains-retryable-fence-test
  ;; An interrupt during a provider's physical stop does not establish that
  ;; its resources drained. Retain its place in the cleanup journal, keep its
  ;; prerequisites open, report the exact suppressed stop error, and preserve
  ;; the interrupted caller's flag so the host can retry intentionally.
  (let [events (atom [])
        attempts (atom 0)
        start-error (ex-info "startup failed" {:test :startup})
        interruption (InterruptedException. "rollback stop interrupted")
        dependency {:biff.core/id :fixture/dependency
                    :biff.core/start (fn [system]
                                       (swap! events conj :start-dependency)
                                       system)
                    :biff.core/stop (fn [_]
                                      (swap! events conj :stop-dependency))}
        provider {:biff.core/id :fixture/provider
                  :biff.core/start (fn [system]
                                     (swap! events conj :start-provider)
                                     system)
                  :biff.core/stop
                  (fn [_]
                    (swap! events conj :stop-provider)
                    (when (= 1 (swap! attempts inc))
                      (throw interruption)))}
        doomed {:biff.core/id :fixture/doomed
                :biff.core/start (fn [_]
                                   (swap! events conj :start-doomed)
                                   (throw start-error))}]
    (try
      (Thread/interrupted)
      (let [error (try (start-checked-fixture! [dependency provider doomed])
                       (catch Throwable failure failure))
            interrupted? (.isInterrupted (Thread/currentThread))
            data (ex-data error)
            handle (:cleanup/handle data)]
        (Thread/interrupted)
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (= :blocked (:rollback/status data)))
        (is (identical? start-error (.getCause ^Throwable error)))
        (is (= [interruption] (vec (.getSuppressed ^Throwable error))))
        (is (= :fixture/provider (:rollback/blocked-module-id data)))
        (is (= [:fixture/dependency :fixture/provider]
               (:rollback/remaining-module-ids data)))
        (is (= [:start-dependency :start-provider :start-doomed :stop-provider]
               @events)
            "An interrupted provider stop must fence earlier dependencies")
        (is interrupted? "A blocked rollback must retain JVM cancellation")
        (is (some? handle))
        (when handle
          (is (= :drained (:status (managed/retry-startup-cleanup! handle))))
          (is (= [:start-dependency :start-provider :start-doomed
                  :stop-provider :stop-provider :stop-dependency]
                 @events))
          (is (= 2 @attempts))))
      (finally
        (Thread/interrupted)))))

(deftest failed-registration-compensation-retains-retryable-cleanup-test
  ;; A duplicate registration fails AFTER allocating the second resource.
  ;; If immediate compensation also fails, that resource must become the
  ;; *first* retryable journal entry. Reporting rollback :drained while it is
  ;; still open would incorrectly authorize teardown of its dependencies.
  (let [events (atom [])
        attempts (atom 0)
        compensation-error (ex-info "second resource still open" {:resource :second})
        dependency {:biff.core/id :fixture/dependency
                    :biff.core/start (fn [s]
                                       (swap! events conj :dependency-start)
                                       s)
                    :biff.core/stop (fn [_]
                                      (swap! events conj :dependency-stop))}
        duplicate {:biff.core/id :fixture/duplicate-module
                   :biff.core/start
                   (fn [_]
                     (managed/acquire-startup-resource!
                      :fixture/resource
                      (fn [] (swap! events conj :first-acquire) :first)
                      (fn [_] (swap! events conj :first-close)))
                     (managed/acquire-startup-resource!
                      :fixture/resource
                      (fn [] (swap! events conj :second-acquire) :second)
                      (fn [_]
                        (swap! events conj :second-close)
                        (when (< (swap! attempts inc) 3)
                          (throw compensation-error)))))
                   :biff.core/stop (fn [_] (swap! events conj :unexpected-stop))}
        failure (thrown-startup #(start-checked-fixture! [dependency duplicate]))
        details (ex-data failure)
        original (.getCause ^Throwable failure)
        handle (:cleanup/handle details)]
    (is (= :duplicate-owned-resource (:reason (ex-data original))))
    (is (= [compensation-error]
           (vec (.getSuppressed ^Throwable original)))
        "The first failed physical close must remain attached to the cause")
    (is (= :blocked (:rollback/status details))
        "Unclosed compensation cannot be falsely reported as drained")
    (is (= :fixture/duplicate-module (:rollback/blocked-module-id details)))
    (is (= :fixture/resource (:rollback/blocked-resource-id details)))
    (is (= [:fixture/dependency :fixture/duplicate-module
            :fixture/duplicate-module]
           (:rollback/remaining-module-ids details))
        "Both registered and failed-compensation resource obligations remain")
    (is (= [:dependency-start :first-acquire :second-acquire
            :second-close :second-close]
           @events)
        "Unreleased second resource fences the first and its dependency")
    (is (some? handle) "Physical compensation needs explicit retry authority")
    (when handle
      (is (= :drained (:status (managed/retry-startup-cleanup! handle))))
      (is (= [:dependency-start :first-acquire :second-acquire
              :second-close :second-close :second-close
              :first-close :dependency-stop]
             @events)
          "Successful retry releases new resource before earlier providers")
      (is (= 3 @attempts)))))

(deftest interrupted-failed-registration-compensation-preserves-flag-test
  ;; A compensating close can fail with InterruptedException after clearing
  ;; the caller's interrupt bit. Startup's primary error is the duplicate
  ;; registration, but that suppressed cancellation must still be preserved.
  (let [events (atom [])
        attempts (atom 0)
        interruption (InterruptedException. "compensation interrupted")
        duplicate {:biff.core/id :fixture/duplicate-module
                   :biff.core/start
                   (fn [_]
                     (managed/acquire-startup-resource!
                      :fixture/resource
                      (fn [] (swap! events conj :first-acquire) :first)
                      (fn [_] (swap! events conj :first-close)))
                     (managed/acquire-startup-resource!
                      :fixture/resource
                      (fn [] (swap! events conj :second-acquire) :second)
                      (fn [_]
                        (swap! events conj :second-close)
                        (when (= 1 (swap! attempts inc))
                          (throw interruption)))))
                   :biff.core/stop (fn [_] (swap! events conj :unexpected-stop))}]
    (try
      (Thread/interrupted)
      (let [failure (thrown-startup #(start-checked-fixture! [duplicate]))
            interrupted? (.isInterrupted (Thread/currentThread))]
        (Thread/interrupted)
        (is (= :drained (:rollback/status (ex-data failure))))
        (is (= :duplicate-owned-resource (:reason (ex-data (.getCause failure)))))
        (is (= [interruption] (vec (.getSuppressed ^Throwable (.getCause failure)))))
        (is (= [:first-acquire :second-acquire :second-close
                :second-close :first-close] @events))
        (is (= 2 @attempts))
        (is interrupted? "Suppressed physical cancellation must retain interrupt"))
      (finally
        (Thread/interrupted)))))

(deftest interrupted-native-provider-stop-preserves-retry-and-init-fence-test
  ;; The native Biff stop chain is attempted BEFORE tracked init-resource
  ;; teardown. A physical provider stop may throw InterruptedException after
  ;; clearing the calling thread's flag. Keep its exact error and cancellation
  ;; signal; do not close its dependent init resource until a later retry.
  (let [events (atom [])
        attempts (atom 0)
        interruption (InterruptedException. "native provider stop interrupted")
        init {:biff.core/id :fixture/init
              managed/init-policy-key :tracked
              :biff.core/init
              (fn [_]
                (managed/acquire-init-resource!
                 :fixture/resource
                 (fn [] (swap! events conj :init-acquire) :resource)
                 (fn [_] (swap! events conj :init-close)))
                {})
              :biff.core/start identity}
        provider {:biff.core/id :fixture/provider
                  :biff.core/start
                  (fn [s]
                    (assoc s :xtdb/connectable :fake
                           :gesso.live/system
                           {:source :fake :closed? (atom false)}))
                  :biff.core/stop
                  (fn [_]
                    (swap! events conj :provider-stop)
                    (when (= 1 (swap! attempts inc))
                      (throw interruption)))}]
    (with-redefs [lifecycle/start! (fn [& _] :worker)
                  lifecycle/retire! (fn [& _]
                                    (swap! events conj :worker-stop)
                                    {:outcome :drained})]
      (let [running (start-checked-fixture! [init provider])]
        (try
          (Thread/interrupted)
          (let [failure (try (biff/stop running)
                             (catch Throwable error error))
                interrupted? (.isInterrupted (Thread/currentThread))]
            (Thread/interrupted)
            (is (identical? interruption failure)
                "Native stop must preserve the actual physical exception")
            (is interrupted?
                "Native stop interruption must remain visible to caller")
            (is (= [:init-acquire :worker-stop :provider-stop] @events)
                "Interrupted provider stop must fence init cleanup"))
          (biff/stop running)
          (is (= :init-close (last @events))
              "Explicit retry must eventually close the init resource")
          (is (= 2 @attempts))
          (finally
            (Thread/interrupted)))))))
