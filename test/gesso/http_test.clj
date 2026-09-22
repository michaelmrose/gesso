(ns gesso.http-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.core :as g]
   [gesso.http :as http]
   [rum.core :as rum]))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- tagged-error
  [tag]
  (ex-info "tagged preflight failure"
           {:error/type :example/preflight
            :tag tag}))

(defn var-guard
  [_]
  :accepted)

(defn var-thunk
  []
  :thunk-result)

(deftest html-response-preserves-legacy-behavior-without-a-guard-test
  (testing "ordinary callers remain unguarded unless an enclosing boundary opts in"
    (is (false? (http/html-response-preflight-bound?)))
    (is (= {:status 200
            :headers {"content-type" "text/html; charset=utf-8"}
            :body "<main><span>ok</span></main>"}
           (http/html-response [:main [:span "ok"]])))
    (is (false? (http/html-response-preflight-bound?)))))

(deftest html-response-preflight-sees-the-exact-body-before-serialization-test
  (let [body (with-meta
               [:main {:data-example "body"} [:span "ok"]]
               {:example/metadata :preserved})
        seen (atom [])
        serialized (atom [])]
    (with-redefs [rum/render-static-markup
                  (fn [rendered]
                    (swap! serialized conj rendered)
                    "SERIALIZED")]
      (is (= {:status 200
              :headers {"content-type" "text/html; charset=utf-8"}
              :body "SERIALIZED"}
             (http/with-html-response-preflight
              (fn [rendered]
                (swap! seen conj rendered)
                {:accepted true})
              #(http/html-response body))))
      (is (= [body] @seen))
      (is (identical? body (first @seen)))
      (is (= [body] @serialized))
      (is (identical? body (first @serialized))))))

(deftest require-html-response-preflight-fails-when-no-guard-is-installed-test
  (let [data (error-data #(http/require-html-response-preflight! [:main]))]
    (is (= http/html-response-preflight-error-type (:error/type data)))
    (is (= :missing-html-response-preflight (:error/kind data)))))

(deftest with-html-response-preflight-rejects-broad-ifn-values-test
  (testing "keywords and maps are callable in Clojure but are not accepted as guards"
    (doseq [guard [:keyword-guard {:map :guard}]]
      (let [data (error-data #(http/with-html-response-preflight guard (fn [] :never)))]
        (is (= http/html-response-preflight-error-type (:error/type data)))
        (is (= :invalid-html-response-preflight (:error/kind data)))
        (is (= :inner (:guard-position data)))
        (is (= guard (:guard data))))))
  (testing "the protected thunk must likewise be an actual function/Var"
    (doseq [thunk [:keyword-thunk {:map :thunk} 42]]
      (let [data (error-data #(http/with-html-response-preflight (fn [_] true) thunk))]
        (is (= http/html-response-preflight-error-type (:error/type data)))
        (is (= :invalid-html-response-thunk (:error/kind data)))
        (is (= thunk (:thunk data)))))))

(deftest vars-containing-functions-are-supported-test
  (is (= :thunk-result
         (http/with-html-response-preflight #'var-guard #'var-thunk)))
  (binding [http/*html-response-preflight* #'var-guard]
    (is (= :accepted
           (http/require-html-response-preflight! [:main])))))

(deftest nested-preflight-guards-compose-outer-first-on-the-same-body-test
  (let [body [:main [:span "same"]]
        events (atom [])]
    (with-redefs [rum/render-static-markup
                  (fn [rendered]
                    (swap! events conj [:serialize rendered])
                    "SERIALIZED")]
      (let [response
            (http/with-html-response-preflight
             (fn [rendered]
               (swap! events conj [:outer rendered])
               :outer-accepted)
             (fn []
               (http/with-html-response-preflight
                (fn [rendered]
                  (swap! events conj [:inner rendered])
                  :inner-accepted)
                (fn []
                  (http/html-response body)))))]
        (is (= "SERIALIZED" (:body response)))
        (is (= [[:outer body]
                [:inner body]
                [:serialize body]]
               @events))
        (is (every? (fn [[_ rendered]]
                      (identical? body rendered))
                    @events))))))

(deftest outer-explicit-rejection-short-circuits-inner-guard-and-serialization-test
  (let [events (atom [])
        data
        (with-redefs [rum/render-static-markup
                      (fn [_]
                        (swap! events conj :serialize)
                        "SHOULD-NOT-RUN")]
          (error-data
           (fn []
             (http/with-html-response-preflight
              (fn [_]
                (swap! events conj :outer)
                false)
              (fn []
                (http/with-html-response-preflight
                 (fn [_]
                   (swap! events conj :inner)
                   true)
                 (fn []
                   (http/html-response [:main]))))))))]
    (is (= [:outer] @events))
    (is (= http/html-response-preflight-error-type (:error/type data)))
    (is (= :html-response-preflight-rejected (:error/kind data)))))

(deftest outer-structured-exception-propagates-and-short-circuits-test
  (let [failure (tagged-error :outer)
        events (atom [])
        caught
        (with-redefs [rum/render-static-markup
                      (fn [_]
                        (swap! events conj :serialize)
                        "SHOULD-NOT-RUN")]
          (try
            (http/with-html-response-preflight
             (fn [_]
               (swap! events conj :outer)
               (throw failure))
             (fn []
               (http/with-html-response-preflight
                (fn [_]
                  (swap! events conj :inner)
                  true)
                (fn []
                  (http/html-response [:main])))))
            nil
            (catch clojure.lang.ExceptionInfo e
              e)))]
    (is (identical? failure caught))
    (is (= {:error/type :example/preflight
            :tag :outer}
           (ex-data caught)))
    (is (= [:outer] @events))))

(deftest inner-rejection-occurs-only-after-outer-acceptance-and-prevents-serialization-test
  (let [events (atom [])
        data
        (with-redefs [rum/render-static-markup
                      (fn [_]
                        (swap! events conj :serialize)
                        "SHOULD-NOT-RUN")]
          (error-data
           (fn []
             (http/with-html-response-preflight
              (fn [_]
                (swap! events conj :outer)
                true)
              (fn []
                (http/with-html-response-preflight
                 (fn [_]
                   (swap! events conj :inner)
                   nil)
                 (fn []
                   (http/html-response [:main]))))))))]
    (is (= [:outer :inner] @events))
    (is (= http/html-response-preflight-error-type (:error/type data)))
    (is (= :html-response-preflight-rejected (:error/kind data)))))

(deftest nested-scope-restores-the-exact-outer-binding-test
  (let [outer (fn [_] :outer)
        inner (fn [_] :inner)
        observed (atom {})]
    (binding [http/*html-response-preflight* outer]
      (swap! observed assoc :before http/*html-response-preflight*)
      (http/with-html-response-preflight
       inner
       (fn []
         (swap! observed assoc :inside http/*html-response-preflight*)
         (is (not (identical? outer http/*html-response-preflight*)))
         (is (http/html-response-preflight-bound?))))
      (swap! observed assoc :after http/*html-response-preflight*))
    (is (identical? outer (:before @observed)))
    (is (identical? outer (:after @observed)))
    (is (not (identical? (:inside @observed) outer)))
    (is (false? (http/html-response-preflight-bound?)))))

(deftest malformed-existing-outer-guard-is-rejected-before-inner-thunk-test
  (let [called? (atom false)
        data
        (binding [http/*html-response-preflight* :malformed-outer]
          (error-data
           (fn []
             (http/with-html-response-preflight
              (fn [_] true)
              (fn []
                (reset! called? true)
                :never)))))]
    (is (false? @called?))
    (is (= http/html-response-preflight-error-type (:error/type data)))
    (is (= :invalid-html-response-preflight (:error/kind data)))
    (is (= :outer (:guard-position data)))
    (is (= :malformed-outer (:guard data)))))

(deftest gesso-core-html-response-alias-observes-the-dynamic-http-guard-test
  (let [body [:article [:p "aliased"]]
        seen (atom [])]
    (with-redefs [rum/render-static-markup (constantly "ALIASED")]
      (let [response
            (http/with-html-response-preflight
             (fn [rendered]
               (swap! seen conj rendered)
               true)
             (fn []
               (g/html-response body)))]
        (is (= "ALIASED" (:body response)))
        (is (= [body] @seen))
        (is (identical? body (first @seen)))))))

(deftest no-content-does-not-cross-the-html-preflight-boundary-test
  (let [calls (atom 0)]
    (is (= {:status 204 :headers {} :body ""}
           (http/with-html-response-preflight
            (fn [_]
              (swap! calls inc)
              true)
            http/no-content)))
    (is (zero? @calls))))
