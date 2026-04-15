(ns gesso.live.bus-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.bus :as bus]))

(def request-matcher
  {:subscription->entries
   (fn [subscription]
     (case (:kind subscription)
       :request [[:request (:id subscription)]]
       :store   [[:store (:id subscription)]]
       []))

   :changed->entries
   (fn [_ctx changed]
     (let [entity-type (get changed :entity/type)
           entity-id   (get changed :entity/id)
           store-id    (get changed :store/id)]
       (cond-> []
         (and (= entity-type :request) entity-id)
         (conj [:request entity-id])

         (and (= entity-type :request) store-id)
         (conj [:store store-id]))))})

(defn mk-subscriber
  [id subscription sent]
  {:subscriber/id id
   :subscription subscription
   :send! (fn [event]
            (swap! sent conj [id event]))})

(deftest bus-from-ctx-test
  (let [live-bus {:x 1}
        ctx {:gesso.live/bus live-bus}]
    (is (= live-bus
           (bus/bus-from-ctx ctx)))))

(deftest empty-state-test
  (let [live-bus (bus/empty-state request-matcher)]
    (is (= request-matcher (:matcher live-bus)))
    (is (= {} @(::bus/subscribers live-bus)))
    (is (= {} @(::bus/indexes live-bus)))))

(deftest subscribe-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        subscriber (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)]
    (bus/subscribe! live-bus subscriber)

    (is (= subscriber
           (get @(::bus/subscribers live-bus) :sub-1)))

    (is (= #{:sub-1}
           (get-in @(::bus/indexes live-bus) [:request "req-1"])))))

(deftest unsubscribe-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        subscriber (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)]
    (bus/subscribe! live-bus subscriber)
    (bus/unsubscribe! live-bus :sub-1)

    (is (nil? (get @(::bus/subscribers live-bus) :sub-1)))
    (is (nil? (get-in @(::bus/indexes live-bus) [:request "req-1"])))))


(deftest replace-subscription-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        subscriber (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)]
    (bus/subscribe! live-bus subscriber)
    (bus/replace-subscription! live-bus :sub-1 {:kind :store :id "store-7"})

    (is (= {:kind :store :id "store-7"}
           (get-in @(::bus/subscribers live-bus) [:sub-1 :subscription])))

    (is (nil? (get-in @(::bus/indexes live-bus) [:request "req-1"])))
    (is (= #{:sub-1}
           (get-in @(::bus/indexes live-bus) [:store "store-7"])))))

(deftest candidate-subscriber-ids-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        sub-1 (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)
        sub-2 (mk-subscriber :sub-2 {:kind :store :id "store-7"} sent)]
    (bus/subscribe! live-bus sub-1)
    (bus/subscribe! live-bus sub-2)

    (is (= #{:sub-1 :sub-2}
           (bus/candidate-subscriber-ids
            live-bus
            {}
            {:entity/type :request
             :entity/id "req-1"
             :store/id "store-7"})))))

(deftest matching-subscriber-ids-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        sub-1 (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)
        sub-2 (mk-subscriber :sub-2 {:kind :store :id "store-7"} sent)]
    (bus/subscribe! live-bus sub-1)
    (bus/subscribe! live-bus sub-2)

    (is (= #{:sub-1 :sub-2}
           (bus/matching-subscriber-ids
            live-bus
            {}
            {:entity/type :request
             :entity/id "req-1"
             :store/id "store-7"})))))

(deftest notify-subscribers-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        event {:event "live-update"
               :changed {:entity/type :request :entity/id "req-1"}}
        sub-1 (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)
        sub-2 (mk-subscriber :sub-2 {:kind :store :id "store-7"} sent)]
    (bus/subscribe! live-bus sub-1)
    (bus/subscribe! live-bus sub-2)

    (let [summary (bus/notify-subscribers! live-bus #{:sub-1 :sub-2} event)]
      (is (= #{:sub-1 :sub-2} (:matched-subscriber-ids summary)))
      (is (= 2 (:delivered-count summary)))
      (is (= [] (:errors summary)))
      (is (= [[:sub-1 event]
              [:sub-2 event]]
             @sent)))))

(deftest publish-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        event {:event "live-update"
               :changed {:entity/type :request
                         :entity/id "req-1"
                         :store/id "store-7"}}
        sub-1 (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)
        sub-2 (mk-subscriber :sub-2 {:kind :store :id "store-7"} sent)
        sub-3 (mk-subscriber :sub-3 {:kind :request :id "req-9"} sent)]
    (bus/subscribe! live-bus sub-1)
    (bus/subscribe! live-bus sub-2)
    (bus/subscribe! live-bus sub-3)

    (let [summary (bus/publish! live-bus event)]
      (is (= #{:sub-1 :sub-2} (:matched-subscriber-ids summary)))
      (is (= 2 (:delivered-count summary)))
      (is (= [[:sub-1 event]
              [:sub-2 event]]
             @sent)))))

(deftest subscribers-and-indexes-snapshot-test
  (let [live-bus (bus/memory-bus request-matcher)
        sent (atom [])
        sub-1 (mk-subscriber :sub-1 {:kind :request :id "req-1"} sent)]
    (bus/subscribe! live-bus sub-1)

    (is (= sub-1
           (get (bus/subscribers-snapshot live-bus) :sub-1)))

    (is (= #{:sub-1}
           (get-in (bus/indexes-snapshot live-bus) [:request "req-1"])))))
