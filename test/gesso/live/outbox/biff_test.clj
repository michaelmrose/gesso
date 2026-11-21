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
        (is (= [assembly (config) #'*modules* start-order]
               (first @calls))))
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
                  lifecycle/stop!
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
            (is (= 1 (:total-dispatch-accepted (lifecycle/status handle))))
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
