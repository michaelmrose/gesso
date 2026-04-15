(ns gesso.live.token-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.token :as token]))

(deftest header-name-test
  (testing "returns the canonical live consistency token header"
    (is (= "x-gesso-live-consistency-token"
           (token/header-name)))))

(deftest param-key-test
  (testing "returns the canonical fallback param key"
    (is (= :consistency-token
           (token/param-key)))))

(deftest extract-request-token-test
  (testing "reads token from header"
    (is (= [:tx 9]
           (token/extract-request-token
            {:headers {"x-gesso-live-consistency-token" [:tx 9]}}))))

  (testing "falls back to params"
    (is (= [:tx 8]
           (token/extract-request-token
            {:params {:consistency-token [:tx 8]}}))))

  (testing "prefers header over params"
    (is (= [:tx 9]
           (token/extract-request-token
            {:headers {"x-gesso-live-consistency-token" [:tx 9]}
             :params {:consistency-token [:tx 8]}}))))

  (testing "returns nil when absent"
    (is (nil? (token/extract-request-token {})))))

(deftest request-token-present-test
  (testing "true when token is in header"
    (is (true? (token/request-token-present?
                {:headers {"x-gesso-live-consistency-token" [:tx 9]}}))))

  (testing "true when token is in params"
    (is (true? (token/request-token-present?
                {:params {:consistency-token [:tx 8]}}))))

  (testing "false when token is absent"
    (is (false? (token/request-token-present? {})))))

(deftest assoc-request-token-test
  (testing "associates token into empty headers map"
    (is (= {:headers {"x-gesso-live-consistency-token" [:tx 9]}}
           (token/assoc-request-token {:headers {}} [:tx 9]))))

  (testing "associates token when headers missing"
    (is (= {:headers {"x-gesso-live-consistency-token" [:tx 9]}}
           (token/assoc-request-token {} [:tx 9]))))

  (testing "preserves other request keys"
    (is (= {:request-method :get
            :headers {"x-gesso-live-consistency-token" [:tx 9]}}
           (token/assoc-request-token {:request-method :get} [:tx 9]))))

  (testing "nil token leaves request unchanged"
    (is (= {:headers {"x-other" "y"}}
           (token/assoc-request-token {:headers {"x-other" "y"}} nil)))))

(deftest dissoc-request-token-test
  (testing "removes token from headers"
    (is (= {:headers {"x-other" "y"}}
           (token/dissoc-request-token
            {:headers {"x-gesso-live-consistency-token" [:tx 9]
                       "x-other" "y"}}))))

  (testing "removes token from params too"
    (is (= {:headers {"x-other" "y"}
            :params {:other 1}}
           (token/dissoc-request-token
            {:headers {"x-gesso-live-consistency-token" [:tx 9]
                       "x-other" "y"}
             :params {:consistency-token [:tx 9]
                      :other 1}}))))

  (testing "is safe when token absent"
    (is (= {:headers {"x-other" "y"}}
           (token/dissoc-request-token
            {:headers {"x-other" "y"}})))))
