(ns gesso.live.match-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.match :as match]))

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

(def confirming-matcher
  (assoc request-matcher
         :matches?
         (fn [_ctx changed subscription]
           (let [entity-id (get changed :entity/id)
                 store-id  (get changed :store/id)]
             (case (:kind subscription)
               :request (= entity-id (:id subscription))
               :store   (= store-id (:id subscription))
               false)))))

(deftest index-entries-test
  (testing "derives index entries from subscription"
    (is (= [[:request "req-1"]]
           (match/index-entries
            request-matcher
            {:kind :request :id "req-1"})))
    (is (= [[:store "store-7"]]
           (match/index-entries
            request-matcher
            {:kind :store :id "store-7"})))))

(deftest candidate-entries-test
  (testing "derives candidate entries from changed value"
    (is (= [[:request "req-1"]
            [:store "store-7"]]
           (match/candidate-entries
            request-matcher
            {}
            {:entity/type :request
             :entity/id "req-1"
             :store/id "store-7"})))))

(deftest confirm-match-test
  (testing "defaults to true when matcher does not provide :matches?"
    (is (true?
         (match/confirm-match?
          request-matcher
          {}
          {:entity/type :request :entity/id "req-1"}
          {:kind :request :id "nope"}))))

  (testing "uses matcher :matches? when provided"
    (is (true?
         (match/confirm-match?
          confirming-matcher
          {}
          {:entity/type :request
           :entity/id "req-1"
           :store/id "store-7"}
          {:kind :request :id "req-1"})))
    (is (false?
         (match/confirm-match?
          confirming-matcher
          {}
          {:entity/type :request
           :entity/id "req-1"
           :store/id "store-7"}
          {:kind :request :id "req-2"})))))

(deftest candidate-subscriber-ids-test
  (testing "unions subscriber ids across candidate entries"
    (let [indexes {:request {"req-1" #{:sub-1 :sub-2}}
                   :store   {"store-7" #{:sub-2 :sub-3}}}]
      (is (= #{:sub-1 :sub-2 :sub-3}
             (match/candidate-subscriber-ids
              indexes
              [[:request "req-1"]
               [:store "store-7"]])))))

  (testing "returns empty set when nothing matches"
    (is (= #{}
           (match/candidate-subscriber-ids
            {}
            [[:request "req-1"]])))))

(deftest matching-subscriber-ids-test
  (testing "returns candidates directly when no exact confirmation exists"
    (let [indexes {:request {"req-1" #{:sub-1}}
                   :store   {"store-7" #{:sub-2}}}
          subscribers {:sub-1 {:subscriber/id :sub-1
                               :subscription {:kind :request :id "req-1"}}
                       :sub-2 {:subscriber/id :sub-2
                               :subscription {:kind :store :id "store-7"}}}]
      (is (= #{:sub-1 :sub-2}
             (match/matching-subscriber-ids
              {:matcher request-matcher
               :indexes indexes
               :subscribers subscribers
               :ctx {}
               :changed {:entity/type :request
                         :entity/id "req-1"
                         :store/id "store-7"}})))))

  (testing "filters candidates through exact confirmation when available"
    (let [indexes {:request {"req-1" #{:sub-1 :sub-2}}}
          subscribers {:sub-1 {:subscriber/id :sub-1
                               :subscription {:kind :request :id "req-1"}}
                       :sub-2 {:subscriber/id :sub-2
                               :subscription {:kind :request :id "req-2"}}}]
      (is (= #{:sub-1}
             (match/matching-subscriber-ids
              {:matcher confirming-matcher
               :indexes indexes
               :subscribers subscribers
               :ctx {}
               :changed {:entity/type :request
                         :entity/id "req-1"}}))))))

(deftest index-subscription-test
  (testing "indexes a subscriber under all matcher-derived entries"
    (let [indexes {}
          subscriber {:subscriber/id :sub-1
                      :subscription {:kind :request :id "req-1"}}
          indexes' (match/index-subscription indexes request-matcher subscriber)]
      (is (= #{:sub-1}
             (get-in indexes' [:request "req-1"])))))

  (testing "supports multiple subscribers in same bucket"
    (let [indexes {:request {"req-1" #{:sub-1}}}
          subscriber {:subscriber/id :sub-2
                      :subscription {:kind :request :id "req-1"}}
          indexes' (match/index-subscription indexes request-matcher subscriber)]
      (is (= #{:sub-1 :sub-2}
             (get-in indexes' [:request "req-1"]))))))

(deftest unindex-subscription-test
  (testing "removes a subscriber from matcher-derived entries"
    (let [indexes {:request {"req-1" #{:sub-1 :sub-2}}}
          subscriber {:subscriber/id :sub-2
                      :subscription {:kind :request :id "req-1"}}
          indexes' (match/unindex-subscription indexes request-matcher subscriber)]
      (is (= #{:sub-1}
             (get-in indexes' [:request "req-1"])))))

  (testing "removes empty buckets"
    (let [indexes {:request {"req-1" #{:sub-1}}}
          subscriber {:subscriber/id :sub-1
                      :subscription {:kind :request :id "req-1"}}
          indexes' (match/unindex-subscription indexes request-matcher subscriber)]
      (is (nil? (get-in indexes' [:request "req-1"]))))))
