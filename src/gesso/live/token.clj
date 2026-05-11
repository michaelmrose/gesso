(ns gesso.live.token
  "Token helpers for gesso.live.

   This namespace owns two related concerns:

   1. Consistency token plumbing
      - canonical request header
      - fallback request param
      - extraction from Ring/Biff-ish request or ctx maps
      - assoc/dissoc helpers for request maps

   2. Subscription token plumbing
      - encode a rich subscription map into an opaque browser token
      - decode the browser token back into a subscription map
      - optionally sign/verify tokens with HMAC-SHA256

   Token payloads are encoded as Transit JSON, then base64url encoded.

   Important:

   Opaque encoding is not authorization.
   HMAC signing is not authorization.

   A decoded subscription must still be authorized by app code before a client
   receives a live stream."
  (:require
   [clojure.string :as str]
   [cognitect.transit :as transit]
   [gesso.live.schema :as schema])
  (:import
   [java.io ByteArrayInputStream ByteArrayOutputStream]
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest]
   [java.util Base64]
   [javax.crypto Mac]
   [javax.crypto.spec SecretKeySpec]))

;; -----------------------------------------------------------------------------
;; Public constants
;; -----------------------------------------------------------------------------

(def consistency-token-header-name
  "Canonical request header used to propagate read/write consistency tokens."
  "x-gesso-live-consistency-token")

(def consistency-token-param-name
  "Fallback query/form param used to propagate read/write consistency tokens."
  "gesso-live-consistency-token")

(def subscription-param-name
  "Default query param name used for encoded subscription tokens."
  "subscription")

(def token-version
  "Current subscription token version."
  "v1")

(def hmac-algorithm
  "HMAC algorithm used for signed subscription tokens."
  "HmacSHA256")

(defn consistency-header-name
  "Return the canonical consistency token request header name."
  []
  consistency-token-header-name)

(defn consistency-param-name
  "Return the fallback consistency token request param name."
  []
  consistency-token-param-name)

(defn subscription-param
  "Return the default subscription query param name."
  []
  subscription-param-name)

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(def ^:private byte-array-class
  (Class/forName "[B"))

(defn- byte-array?
  [x]
  (instance? byte-array-class x))

(defn- utf8-bytes
  ^bytes
  [s]
  (.getBytes (str s) StandardCharsets/UTF_8))

(defn- non-blank-string?
  [x]
  (and (string? x)
       (not (str/blank? x))))

(defn- present-token
  "Normalize token-like boundary values.

   Blank strings are treated as missing."
  [x]
  (cond
    (nil? x)
    nil

    (string? x)
    (when-not (str/blank? x)
      x)

    :else
    x))

(defn- request-map
  "Return the Ring request map from ctx-or-request.

   In many Gesso/Biff call paths, ctx itself is the request-like map. In others,
   the request may be nested under :request."
  [ctx-or-request]
  (or (:request ctx-or-request)
      ctx-or-request))

(defn- header-value
  [headers header-name]
  (present-token
   (or (get headers header-name)
       (get headers (str/lower-case header-name))
       (get headers (str/upper-case header-name))
       (get headers (keyword header-name))
       (get headers (keyword (str/lower-case header-name))))))

(defn- param-value
  [params param-name]
  (present-token
   (or (get params param-name)
       (get params (keyword param-name)))))

(defn- request-param-value
  "Search all common Ring request param maps.

   Do not use (or (:params req) (:query-params req) (:form-params req)), because
   an existing but empty :params map would otherwise prevent checking query/form
   params."
  [req param-name]
  (or (param-value (:params req) param-name)
      (param-value (:query-params req) param-name)
      (param-value (:form-params req) param-name)))

(defn- dissoc-existing-submap
  "Dissoc keys from an existing request submap.

   Does not create absent request submaps."
  [m k & ks]
  (if (contains? m k)
    (update m k #(apply dissoc (or % {}) ks))
    m))

(defn- require-secret!
  [secret]
  (when (or (nil? secret)
            (and (string? secret)
                 (str/blank? secret)))
    (throw
     (ex-info "HMAC secret is required for gesso.live subscription token signing."
              {})))
  secret)

(defn- secret-bytes
  ^bytes
  [secret]
  (let [secret' (require-secret! secret)]
    (cond
      (byte-array? secret')
      secret'

      :else
      (utf8-bytes secret'))))

;; -----------------------------------------------------------------------------
;; Consistency token request/ctx helpers
;; -----------------------------------------------------------------------------

(defn extract-consistency-token
  "Extract a consistency token from a Ring/Biff-ish request or ctx map.

   Lookup order:
   1. canonical header
   2. params/query-params/form-params

   Blank string values are treated as missing.

   Accepts either a raw Ring request map or a ctx map containing :request."
  [ctx-or-request]
  (let [req     (request-map ctx-or-request)
        headers (:headers req)]
    (or (header-value headers consistency-token-header-name)
        (request-param-value req consistency-token-param-name))))

(defn assoc-consistency-token
  "Assoc a consistency token onto a Ring request map's headers.

   Returns the request unchanged if token is nil or blank."
  [request token]
  (if-let [token' (present-token token)]
    (assoc-in request [:headers consistency-token-header-name] token')
    request))

(defn dissoc-consistency-token
  "Remove the canonical consistency token header and fallback param from a Ring
   request map.

   Does not create nil :headers/:params/:query-params/:form-params keys when
   those submaps are absent."
  [request]
  (-> request
      (dissoc-existing-submap :headers
                              consistency-token-header-name
                              (str/lower-case consistency-token-header-name)
                              (str/upper-case consistency-token-header-name)
                              (keyword consistency-token-header-name)
                              (keyword (str/lower-case consistency-token-header-name)))
      (dissoc-existing-submap :params
                              consistency-token-param-name
                              (keyword consistency-token-param-name))
      (dissoc-existing-submap :query-params
                              consistency-token-param-name
                              (keyword consistency-token-param-name))
      (dissoc-existing-submap :form-params
                              consistency-token-param-name
                              (keyword consistency-token-param-name))))

;; -----------------------------------------------------------------------------
;; Base64url helpers
;; -----------------------------------------------------------------------------

(defn base64url-encode
  "Base64url encode a byte array without padding."
  [^bytes bytes]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes))

(defn base64url-decode
  "Base64url decode a string into a byte array."
  ^bytes
  [s]
  (.decode (Base64/getUrlDecoder) (str s)))

;; -----------------------------------------------------------------------------
;; Transit payload helpers
;; -----------------------------------------------------------------------------

(defn- encode-transit-bytes
  ^bytes
  [x]
  (let [out    (ByteArrayOutputStream.)
        writer (transit/writer out :json)]
    (transit/write writer x)
    (.toByteArray out)))

(defn- decode-transit-bytes
  [^bytes bytes]
  (let [in     (ByteArrayInputStream. bytes)
        reader (transit/reader in :json)]
    (transit/read reader)))

(defn- encode-payload
  [x]
  (base64url-encode (encode-transit-bytes x)))

(defn- decode-payload
  [payload]
  (try
    (decode-transit-bytes (base64url-decode payload))
    (catch Exception e
      (throw
       (ex-info "Invalid gesso.live subscription token payload."
                {:payload payload}
                e)))))

;; -----------------------------------------------------------------------------
;; HMAC signing helpers
;; -----------------------------------------------------------------------------

(defn- hmac-sha256
  ^bytes
  [secret message]
  (let [secret' (secret-bytes secret)
        mac     (Mac/getInstance hmac-algorithm)]
    (.init mac (SecretKeySpec. secret' hmac-algorithm))
    (.doFinal mac (utf8-bytes message))))

(defn- sign-bytes
  ^bytes
  [secret message]
  (hmac-sha256 secret message))

(defn- sign
  [secret message]
  (base64url-encode (sign-bytes secret message)))

(defn- constant-time-bytes=
  [^bytes a ^bytes b]
  (MessageDigest/isEqual a b))

(defn- signing-input
  [version payload]
  (str version "." payload))

;; -----------------------------------------------------------------------------
;; Subscription token parts
;; -----------------------------------------------------------------------------

(defn- validate-token-part!
  [part-name part token]
  (when-not (non-blank-string? part)
    (throw
     (ex-info "Malformed gesso.live subscription token."
              {:reason :blank-token-part
               :part part-name
               :token token})))
  part)

(defn token-parts
  "Split a subscription token into a parts map.

   Returns:
     {:version ...
      :payload ...
      :signature ...}

   Throws on malformed token shape or blank parts."
  [token]
  (let [token' (str token)
        parts  (str/split token' #"\." -1)]
    (case (count parts)
      2
      (let [[version payload] parts]
        {:version (validate-token-part! :version version token)
         :payload (validate-token-part! :payload payload token)
         :signature nil})

      3
      (let [[version payload signature] parts]
        {:version (validate-token-part! :version version token)
         :payload (validate-token-part! :payload payload token)
         :signature (validate-token-part! :signature signature token)})

      (throw
       (ex-info "Malformed gesso.live subscription token."
                {:reason :wrong-part-count
                 :token token
                 :part-count (count parts)
                 :parts parts})))))

(defn signed-token?
  "Return true when token contains a signature part."
  [token]
  (some? (:signature (token-parts token))))

;; -----------------------------------------------------------------------------
;; Subscription token encoding/decoding
;; -----------------------------------------------------------------------------

(defn encode-subscription
  "Encode a subscription map into an opaque browser token.

   Options:
   - :secret optional. When present, the token is HMAC-SHA256 signed.

   Unsigned token format:
     v1.<payload>

   Signed token format:
     v1.<payload>.<signature>

   The payload is Transit JSON, base64url encoded without padding.

   Signing is tamper detection, not authorization. The app must still authorize
   the decoded subscription."
  ([subscription]
   (encode-subscription subscription nil))
  ([subscription {:keys [secret]}]
   (schema/validate-subscription! subscription)
   (let [payload (encode-payload subscription)
         input   (signing-input token-version payload)]
     (if (some? secret)
       (str input "." (sign secret input))
       input))))

(defn verify-signature!
  "Verify a subscription token signature.

   Returns parts when valid.

   Throws when:
   - token is unsigned and :require-signed? is true
   - token is signed but no secret is supplied
   - signature is invalid"
  [{:keys [version payload signature] :as parts}
   {:keys [secret require-signed?]}]
  (cond
    (and require-signed? (nil? signature))
    (throw
     (ex-info "Unsigned gesso.live subscription token is not allowed."
              {:version version
               :payload payload}))

    (nil? signature)
    parts

    :else
    (let [input          (signing-input version payload)
          expected-bytes (sign-bytes secret input)
          actual-bytes   (try
                           (base64url-decode signature)
                           (catch Exception e
                             (throw
                              (ex-info "Malformed gesso.live subscription token signature."
                                       {:version version
                                        :payload payload
                                        :signature signature}
                                       e))))]
      (when-not (constant-time-bytes= expected-bytes actual-bytes)
        (throw
         (ex-info "Invalid gesso.live subscription token signature."
                  {:version version
                   :payload payload})))
      parts)))

(defn decode-subscription
  "Decode an opaque browser token into a subscription map.

   Options:
   - :secret optional HMAC secret.
   - :require-signed? when true, unsigned tokens are rejected.

   If a token contains a signature, a secret must be supplied.

   The decoded subscription is validated against
   :gesso.live/subscription before being returned."
  ([token]
   (decode-subscription token nil))
  ([token opts]
   (let [{:keys [version payload] :as parts} (token-parts token)]
     (when-not (= token-version version)
       (throw
        (ex-info "Unsupported gesso.live subscription token version."
                 {:expected token-version
                  :actual version
                  :token token})))
     (verify-signature! parts opts)
     (schema/validate-subscription! (decode-payload payload)))))

(defn try-decode-subscription
  "Decode a subscription token, returning nil instead of throwing on failure.

   This is useful at boundaries where the caller wants to fail closed."
  ([token]
   (try-decode-subscription token nil))
  ([token opts]
   (try
     (decode-subscription token opts)
     (catch Exception _
       nil))))

;; -----------------------------------------------------------------------------
;; Subscription token request helpers
;; -----------------------------------------------------------------------------

(defn extract-subscription-token
  "Extract the encoded subscription token from a request or ctx map.

   Accepts either a raw Ring request map or a ctx map containing :request.

   Blank string values are treated as missing."
  ([ctx-or-request]
   (extract-subscription-token ctx-or-request subscription-param-name))
  ([ctx-or-request param-name]
   (request-param-value (request-map ctx-or-request) param-name)))

(defn assoc-subscription-token
  "Assoc an encoded subscription token into a request map's params.

   Mostly useful for tests and internal plumbing.

   Returns the request unchanged if token is nil or blank."
  ([request token]
   (assoc-subscription-token request subscription-param-name token))
  ([request param-name token]
   (if-let [token' (present-token token)]
     (assoc-in request [:params param-name] token')
     request)))

(defn dissoc-subscription-token
  "Remove the encoded subscription token from params/query-params/form-params.

   Does not create nil submaps when those submaps are absent."
  ([request]
   (dissoc-subscription-token request subscription-param-name))
  ([request param-name]
   (-> request
       (dissoc-existing-submap :params param-name (keyword param-name))
       (dissoc-existing-submap :query-params param-name (keyword param-name))
       (dissoc-existing-submap :form-params param-name (keyword param-name)))))
