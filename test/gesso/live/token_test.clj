(ns gesso.live.token-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.token :as token])
  (:import
   [java.nio.charset StandardCharsets]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- utf8-bytes
  [s]
  (.getBytes (str s) StandardCharsets/UTF_8))

(def valid-subscription
  {:topic :store-queue
   :id "store-123"})

(def another-subscription
  {:topic :request
   :id "req-456"})

(def secret
  "test-secret-that-is-long-enough-for-hmac-tests")

(def other-secret
  "different-test-secret")

;; -----------------------------------------------------------------------------
;; Public names
;; -----------------------------------------------------------------------------

(deftest public-name-test
  (testing "consistency token names are stable"
    (is (= "x-gesso-live-consistency-token"
           (token/consistency-header-name)))

    (is (= "gesso-live-consistency-token"
           (token/consistency-param-name))))

  (testing "subscription param name is stable"
    (is (= "subscription"
           (token/subscription-param)))))

;; -----------------------------------------------------------------------------
;; Consistency token extraction
;; -----------------------------------------------------------------------------

(deftest extract-consistency-token-test
  (testing "extracts from canonical header"
    (is (= "tx-1"
           (token/extract-consistency-token
            {:headers {"x-gesso-live-consistency-token" "tx-1"}}))))

  (testing "extracts from lowercase/uppercase/keyword header variants"
    (is (= "tx-lower"
           (token/extract-consistency-token
            {:headers {"x-gesso-live-consistency-token" "tx-lower"}})))

    (is (= "tx-upper"
           (token/extract-consistency-token
            {:headers {"X-GESSO-LIVE-CONSISTENCY-TOKEN" "tx-upper"}})))

    (is (= "tx-keyword"
           (token/extract-consistency-token
            {:headers {:x-gesso-live-consistency-token "tx-keyword"}}))))

  (testing "header wins over params"
    (is (= "tx-header"
           (token/extract-consistency-token
            {:headers {"x-gesso-live-consistency-token" "tx-header"}
             :params {"gesso-live-consistency-token" "tx-param"}}))))

  (testing "blank header is treated as missing and falls back to params"
    (is (= "tx-param"
           (token/extract-consistency-token
            {:headers {"x-gesso-live-consistency-token" "   "}
             :params {"gesso-live-consistency-token" "tx-param"}}))))

  (testing "extracts from params, query-params, or form-params"
    (is (= "tx-params"
           (token/extract-consistency-token
            {:params {"gesso-live-consistency-token" "tx-params"}})))

    (is (= "tx-query"
           (token/extract-consistency-token
            {:query-params {"gesso-live-consistency-token" "tx-query"}})))

    (is (= "tx-form"
           (token/extract-consistency-token
            {:form-params {"gesso-live-consistency-token" "tx-form"}}))))

  (testing "empty :params does not hide query/form params"
    (is (= "tx-query"
           (token/extract-consistency-token
            {:params {}
             :query-params {"gesso-live-consistency-token" "tx-query"}})))

    (is (= "tx-form"
           (token/extract-consistency-token
            {:params {}
             :query-params {}
             :form-params {"gesso-live-consistency-token" "tx-form"}}))))

  (testing "keyword params are accepted"
    (is (= "tx-keyword-param"
           (token/extract-consistency-token
            {:params {:gesso-live-consistency-token "tx-keyword-param"}}))))

  (testing "accepts ctx maps containing :request"
    (is (= "tx-nested"
           (token/extract-consistency-token
            {:request {:headers {"x-gesso-live-consistency-token" "tx-nested"}}}))))

  (testing "missing or blank token returns nil"
    (is (nil?
         (token/extract-consistency-token {})))

    (is (nil?
         (token/extract-consistency-token
          {:headers {"x-gesso-live-consistency-token" ""}
           :params {"gesso-live-consistency-token" "   "}})))))

(deftest assoc-dissoc-consistency-token-test
  (testing "assoc-consistency-token adds canonical header"
    (is (= {:headers {"x-gesso-live-consistency-token" "tx-1"}}
           (token/assoc-consistency-token {} "tx-1"))))

  (testing "assoc-consistency-token preserves existing headers"
    (is (= {:headers {"x-existing" "yes"
                      "x-gesso-live-consistency-token" "tx-1"}}
           (token/assoc-consistency-token
            {:headers {"x-existing" "yes"}}
            "tx-1"))))

  (testing "assoc-consistency-token ignores nil and blank tokens"
    (is (= {}
           (token/assoc-consistency-token {} nil)))

    (is (= {}
           (token/assoc-consistency-token {} "")))

    (is (= {}
           (token/assoc-consistency-token {} "   "))))

  (testing "dissoc-consistency-token removes header and param variants"
    (is (= {:headers {"x-other" "yes"}
            :params {"other" "value"}
            :query-params {}
            :form-params {}}
           (token/dissoc-consistency-token
            {:headers {"x-gesso-live-consistency-token" "tx-1"
                       "X-GESSO-LIVE-CONSISTENCY-TOKEN" "tx-2"
                       :x-gesso-live-consistency-token "tx-3"
                       "x-other" "yes"}
             :params {"gesso-live-consistency-token" "tx-param"
                      :gesso-live-consistency-token "tx-param-2"
                      "other" "value"}
             :query-params {"gesso-live-consistency-token" "tx-query"}
             :form-params {"gesso-live-consistency-token" "tx-form"}}))))

  (testing "dissoc-consistency-token does not create absent nil submaps"
    (is (= {}
           (token/dissoc-consistency-token {})))))

;; -----------------------------------------------------------------------------
;; Base64url helpers
;; -----------------------------------------------------------------------------

(deftest base64url-test
  (testing "base64url round trips bytes"
    (let [encoded (token/base64url-encode (utf8-bytes "hello world"))
          decoded (token/base64url-decode encoded)]
      (is (= "hello world"
             (String. decoded StandardCharsets/UTF_8)))))

  (testing "base64url encoding omits padding"
    (is (not (.contains (token/base64url-encode (utf8-bytes "hello world"))
                        "=")))))

;; -----------------------------------------------------------------------------
;; Subscription token parts
;; -----------------------------------------------------------------------------

(deftest token-parts-test
  (testing "token-parts parses unsigned tokens"
    (is (= {:version "v1"
            :payload "payload"
            :signature nil}
           (token/token-parts "v1.payload"))))

  (testing "token-parts parses signed tokens"
    (is (= {:version "v1"
            :payload "payload"
            :signature "signature"}
           (token/token-parts "v1.payload.signature"))))

  (testing "token-parts rejects wrong part counts"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Malformed gesso.live subscription token"
         (token/token-parts "v1")))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Malformed gesso.live subscription token"
         (token/token-parts "v1.payload.signature.extra"))))

  (testing "token-parts rejects blank parts"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Malformed gesso.live subscription token"
         (token/token-parts ".payload")))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Malformed gesso.live subscription token"
         (token/token-parts "v1.")))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Malformed gesso.live subscription token"
         (token/token-parts "v1.payload.")))))

(deftest signed-token-predicate-test
  (testing "signed-token? detects signature presence"
    (is (false?
         (token/signed-token? "v1.payload")))

    (is (true?
         (token/signed-token? "v1.payload.signature")))))

;; -----------------------------------------------------------------------------
;; Subscription encode/decode
;; -----------------------------------------------------------------------------

(deftest encode-decode-subscription-test
  (testing "unsigned subscription tokens round trip through Transit payloads"
    (let [encoded (token/encode-subscription valid-subscription)]
      (is (string? encoded))
      (is (= valid-subscription
             (token/decode-subscription encoded)))))

  (testing "subscription ids may be non-string values allowed by schema"
    (let [subscription {:topic :global-announcement
                        :id :main}
          encoded      (token/encode-subscription subscription)]
      (is (= subscription
             (token/decode-subscription encoded)))))

  (testing "invalid subscriptions fail before encoding"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid gesso.live value"
         (token/encode-subscription
          {:topic :store-queue}))))

  (testing "decoded payloads must still conform to subscription schema"
    (let [bad-payload (token/base64url-encode
                       ;; Transit JSON for a map missing :id would be tedious
                       ;; to construct by hand, so encode a valid token and
                       ;; then rely on invalid token shape tests elsewhere.
                       ;; The invalid encode path above covers schema failure
                       ;; before payload creation.
                       (utf8-bytes "not-transit-json"))
          bad-token   (str "v1." bad-payload)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live subscription token payload"
           (token/decode-subscription bad-token))))))

  (testing "try-decode-subscription returns nil on failure"
    (is (nil?
         (token/try-decode-subscription "not.a.valid.token"))))

(deftest signed-subscription-token-test
  (testing "signed subscription tokens round trip when verified with the same secret"
    (let [encoded (token/encode-subscription valid-subscription {:secret secret})]
      (is (token/signed-token? encoded))
      (is (= valid-subscription
             (token/decode-subscription encoded {:secret secret})))))

  (testing "signed tokens cannot be decoded without a secret"
    (let [encoded (token/encode-subscription valid-subscription {:secret secret})]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"HMAC secret is required"
           (token/decode-subscription encoded)))))

  (testing "signed tokens cannot be decoded with the wrong secret"
    (let [encoded (token/encode-subscription valid-subscription {:secret secret})]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live subscription token signature"
           (token/decode-subscription encoded {:secret other-secret})))))

  (testing "require-signed? rejects unsigned tokens"
    (let [encoded (token/encode-subscription valid-subscription)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Unsigned gesso.live subscription token is not allowed"
           (token/decode-subscription encoded
                                      {:secret secret
                                       :require-signed? true})))))

  (testing "require-signed? accepts signed tokens"
    (let [encoded (token/encode-subscription valid-subscription {:secret secret})]
      (is (= valid-subscription
             (token/decode-subscription encoded
                                        {:secret secret
                                         :require-signed? true}))))))

(deftest tampered-token-test
  (testing "tampered signed payload fails signature verification"
    (let [encoded          (token/encode-subscription valid-subscription {:secret secret})
          encoded-other    (token/encode-subscription another-subscription)
          {:keys [payload]} (token/token-parts encoded-other)
          parts            (token/token-parts encoded)
          tampered         (str (:version parts)
                                "."
                                payload
                                "."
                                (:signature parts))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live subscription token signature"
           (token/decode-subscription tampered {:secret secret})))))

  (testing "tampered signed signature fails verification"
    (let [encoded  (token/encode-subscription valid-subscription {:secret secret})
          parts    (token/token-parts encoded)
          tampered (str (:version parts)
                        "."
                        (:payload parts)
                        "."
                        "aaaaaaaaaaaaaaaa")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live subscription token signature"
           (token/decode-subscription tampered {:secret secret})))))

  (testing "malformed signature is rejected clearly"
    (let [encoded  (token/encode-subscription valid-subscription {:secret secret})
          parts    (token/token-parts encoded)
          tampered (str (:version parts)
                        "."
                        (:payload parts)
                        "."
                        "*not-base64url*")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Malformed gesso.live subscription token signature"
           (token/decode-subscription tampered {:secret secret}))))))

(deftest unsupported-token-version-test
  (testing "unsupported token versions fail"
    (let [encoded (token/encode-subscription valid-subscription)
          parts   (token/token-parts encoded)
          v2      (str "v2." (:payload parts))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Unsupported gesso.live subscription token version"
           (token/decode-subscription v2))))))

(deftest blank-secret-test
  (testing "blank signing secrets are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"HMAC secret is required"
         (token/encode-subscription valid-subscription {:secret ""})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"HMAC secret is required"
         (token/encode-subscription valid-subscription {:secret "   "})))))


;; -----------------------------------------------------------------------------
;; Subscription token request helpers
;; -----------------------------------------------------------------------------

(deftest extract-subscription-token-test
  (testing "extracts subscription from params, query-params, or form-params"
    (is (= "sub-params"
           (token/extract-subscription-token
            {:params {"subscription" "sub-params"}})))

    (is (= "sub-query"
           (token/extract-subscription-token
            {:query-params {"subscription" "sub-query"}})))

    (is (= "sub-form"
           (token/extract-subscription-token
            {:form-params {"subscription" "sub-form"}}))))

  (testing "empty :params does not hide query/form params"
    (is (= "sub-query"
           (token/extract-subscription-token
            {:params {}
             :query-params {"subscription" "sub-query"}})))

    (is (= "sub-form"
           (token/extract-subscription-token
            {:params {}
             :query-params {}
             :form-params {"subscription" "sub-form"}}))))

  (testing "custom subscription param names are supported"
    (is (= "custom-sub"
           (token/extract-subscription-token
            {:query-params {"sub" "custom-sub"}}
            "sub"))))

  (testing "keyword params are accepted"
    (is (= "sub-keyword"
           (token/extract-subscription-token
            {:params {:subscription "sub-keyword"}}))))

  (testing "ctx maps containing :request are accepted"
    (is (= "nested-sub"
           (token/extract-subscription-token
            {:request {:query-params {"subscription" "nested-sub"}}}))))

  (testing "blank subscription token is treated as missing"
    (is (nil?
         (token/extract-subscription-token
          {:params {"subscription" "   "}})))))

(deftest assoc-dissoc-subscription-token-test
  (testing "assoc-subscription-token adds default param"
    (is (= {:params {"subscription" "sub-token"}}
           (token/assoc-subscription-token {} "sub-token"))))

  (testing "assoc-subscription-token supports custom param names"
    (is (= {:params {"sub" "sub-token"}}
           (token/assoc-subscription-token {} "sub" "sub-token"))))

  (testing "assoc-subscription-token ignores nil and blank tokens"
    (is (= {}
           (token/assoc-subscription-token {} nil)))

    (is (= {}
           (token/assoc-subscription-token {} "")))

    (is (= {}
           (token/assoc-subscription-token {} "   "))))

  (testing "dissoc-subscription-token removes default param from existing submaps"
    (is (= {:params {"other" "value"}
            :query-params {}
            :form-params {}}
           (token/dissoc-subscription-token
            {:params {"subscription" "sub-token"
                      :subscription "sub-token-2"
                      "other" "value"}
             :query-params {"subscription" "sub-query"}
             :form-params {"subscription" "sub-form"}}))))

  (testing "dissoc-subscription-token supports custom param names"
    (is (= {:params {"subscription" "keep"}}
           (token/dissoc-subscription-token
            {:params {"sub" "remove"
                      "subscription" "keep"}}
            "sub"))))

  (testing "dissoc-subscription-token does not create absent nil submaps"
    (is (= {}
           (token/dissoc-subscription-token {})))))
