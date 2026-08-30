(ns gesso.live.progression.http-test
  "Adversarial transport-boundary tests for browser-supplied Live progression.

   The canonical gesso-live-progression header is an untrusted minimum-read
   request. These tests deliberately keep that contract narrower than authority:

   - valid browser requirements round-trip portably;
   - malformed, ambiguous, or authority-looking wire data fails closed;
   - browser metadata can strengthen but never weaken an already-established
     trusted server requirement;
   - arbitrary opaque basis shape remains opaque transport data and cannot
     overwrite trusted request identity/authority fields.

   Authority-specific interpretation of an opaque basis belongs to the trusted
   consistency/model layer, not this HTTP codec."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.progression :as progression]
   [gesso.live.progression.http :as progression.http]))

(def basis-a
  {:tx-id 10
   :system-time "2026-08-29T10:00:00Z"})

(def basis-b
  {:tx-id 11
   :system-time "2026-08-29T10:00:01Z"})

(def browser-looking-basis
  {:tx-id 999999999
   :system-time "2099-01-01T00:00:00Z"
   :principal "browser-says-admin"
   :authority true})

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) error
      (ex-data error))))

(defn- error-kind
  [f]
  (:error/kind (error-data f)))

(defn- uri-encode
  [value]
  #?(:clj
     (-> (java.net.URLEncoder/encode
          (str value)
          (.name java.nio.charset.StandardCharsets/UTF_8))
         (str/replace "+" "%20"))
     :cljs
     (js/encodeURIComponent (str value))))

(defn- encode-wire
  [wire]
  (uri-encode (pr-str wire)))

(defn- request
  [header-value]
  {:headers
   (cond-> {}
     (some? header-value)
     (assoc progression.http/request-header-name
            header-value))})

(defn- request-with-requirement
  [requirement]
  (request
   (progression.http/encode-request-progression
    requirement)))

(deftest canonical-http-vocabulary-is-explicit
  (is (= "gesso-live-progression"
         progression.http/request-header-name))
  (is (= :gesso.live/progression
         progression.http/canonical-context-key)))

(deftest requirement-header-roundtrip-is-portable-and-header-safe
  (let [requirement
        (progression/requirement-from-bases
         [basis-b
          basis-a
          "basis with spaces\r\nand header-looking: text"])

        encoded
        (progression.http/encode-request-progression
         requirement)]

    (is (string? encoded))
    (is (not (str/blank? encoded)))

    (testing "raw HTTP control characters never survive the transport encoding"
      (is (not (str/includes? encoded "\r")))
      (is (not (str/includes? encoded "\n")))
      (is (not (str/includes? encoded " "))))

    (is (= requirement
           (progression.http/decode-request-progression
            encoded)))

    (is (= requirement
           (progression.http/progression-from-request
            (request encoded))))

    (is (= requirement
           (progression.http/progression-from-request
            {:request (request encoded)
             :unrelated :ctx-value})))))

(deftest nil-header-means-no-browser-minimum-read-requirement
  (is (nil?
       (progression.http/encode-request-progression
        nil)))
  (is (nil?
       (progression.http/decode-request-progression
        nil)))
  (is (nil?
       (progression.http/progression-from-request
        {:headers {}})))
  (is (nil?
       (progression.http/progression-from-request
        {}))))

(deftest present-malformed-header-fails-closed-at-each-transport-layer
  (testing "blank present values are invalid rather than equivalent to absence"
    (doseq [value ["" "   " "\t"]]
      (is (= :invalid-header-value
             (error-kind
              #(progression.http/decode-request-progression
                value))))))

  (testing "invalid URI-component encoding is rejected before EDN parsing"
    (is (= :invalid-header-encoding
           (error-kind
            #(progression.http/decode-request-progression
              "%ZZ")))))

  (testing "decoded text must be readable EDN"
    (is (= :invalid-header-edn
           (error-kind
            #(progression.http/decode-request-progression
              (uri-encode "{"))))))

  (testing "readable EDN still has to satisfy the closed progression wire"
    (doseq [wire
            [{:not :progression}
             {:gesso.live.progression/type
              progression/requirement-wire-type
              :gesso.live.progression/version
              progression/progression-version
              :bases []}
             {:gesso.live.progression/type
              progression/requirement-wire-type
              :gesso.live.progression/version
              progression/progression-version
              :bases [nil]}]]
      (is (= :invalid-progression-wire
             (error-kind
              #(progression.http/decode-request-progression
                (encode-wire wire))))))))

(deftest progression-wire-cannot-smuggle-top-level-authority-or-principal-fields
  (let [valid-wire
        (progression/requirement->wire
         (progression/requirement basis-a))]

    (doseq [forged-wire
            [(assoc valid-wire
                    :authority true)
             (assoc valid-wire
                    :principal "browser-selected-principal")
             (assoc valid-wire
                    :provenance :authoritative)
             (assoc valid-wire
                    :role :admin)
             (assoc valid-wire
                    :operation :grant-admin)]]
      (is (= :invalid-progression-wire
             (error-kind
              #(progression.http/decode-request-progression
                (encode-wire forged-wire))))))))

(deftest header-name-is-case-insensitive-but-conflicting-duplicates-fail-closed
  (let [a
        (progression.http/encode-request-progression
         (progression/requirement basis-a))

        b
        (progression.http/encode-request-progression
         (progression/requirement basis-b))]

    (is (= a
           (progression.http/request-header-value
            {:headers
             {"Gesso-Live-Progression" a}})))

    (is (= a
           (progression.http/request-header-value
            {:headers
             {"Gesso-Live-Progression" a
              :gesso-live-progression a}})))

    (is (= :conflicting-header-values
           (error-kind
            #(progression.http/request-header-value
              {:headers
               {"Gesso-Live-Progression" a
                :gesso-live-progression b}}))))))

(deftest malformed-ring-boundary-shapes-fail-closed
  (is (= :invalid-map
         (error-kind
          #(progression.http/request-map
            nil))))

  (is (= :invalid-map
         (error-kind
          #(progression.http/request-map
            {:request "not-a-ring-request"}))))

  (is (= :invalid-map
         (error-kind
          #(progression.http/request-header-value
            {:headers
             ["not" "a" "header-map"]})))))

(deftest browser-requirement-can-only-strengthen-trusted-server-requirement
  (let [server-a
        (progression/requirement basis-a)

        browser-b
        (progression/requirement basis-b)

        ctx
        {:authenticated-principal :server/helper-1
         :server-authority :trusted
         progression.http/canonical-context-key server-a
         :headers
         {progression.http/request-header-name
          (progression.http/encode-request-progression
           browser-b)}}

        bound
        (progression.http/bind-request-progression
         ctx)

        combined
        (get bound
             progression.http/canonical-context-key)]

    (is (= #{basis-a basis-b}
           (progression/required-bases combined)))

    (is (progression/covers? combined server-a)
        "Browser metadata must never weaken the server-established minimum read.")

    (is (progression/covers? combined browser-b)
        "The validated browser minimum-read request is conservatively added.")

    (is (= :server/helper-1
           (:authenticated-principal bound))
        "Progression binding cannot replace trusted principal identity.")

    (is (= :trusted
           (:server-authority bound))
        "Progression binding cannot replace trusted authority metadata.")))

(deftest duplicate-browser-requirement-cannot-expand-or-weaken-server-requirement
  (let [server-a
        (progression/requirement basis-a)

        bound
        (progression.http/bind-request-progression
         {progression.http/canonical-context-key server-a
          :headers
          {progression.http/request-header-name
           (progression.http/encode-request-progression
            server-a)}})]

    (is (= server-a
           (get bound
                progression.http/canonical-context-key)))))

(deftest absent-browser-header-does-not-rewrite-existing-context
  (let [server-a
        (progression/requirement basis-a)

        ctx
        {:authenticated-principal :server/helper-1
         progression.http/canonical-context-key server-a
         :headers {}}

        bound
        (progression.http/bind-request-progression
         ctx)]

    (is (= ctx bound))))

(deftest opaque-browser-basis-remains-minimum-read-data-not-server-authority
  (let [server-a
        (progression/requirement basis-a)

        browser-request
        (progression/requirement browser-looking-basis)

        ctx
        {:authenticated-principal :server/helper-1
         :server-authority :trusted
         progression.http/canonical-context-key server-a
         :headers
         {progression.http/request-header-name
          (progression.http/encode-request-progression
           browser-request)}}

        bound
        (progression.http/bind-request-progression
         ctx)

        combined
        (get bound
             progression.http/canonical-context-key)]

    (testing
     "generic progression deliberately does not infer semantics from opaque basis shape"
      (is (= #{basis-a browser-looking-basis}
             (progression/required-bases combined))))

    (testing
     "opaque basis contents cannot escape into trusted request identity/authority"
      (is (= :server/helper-1
             (:authenticated-principal bound)))
      (is (= :trusted
             (:server-authority bound)))
      (is (not (contains? bound :principal)))
      (is (not (contains? bound :authority))))

    (testing
     "the resulting requirement is conservatively stronger than trusted server state"
      (is (progression/covers? combined server-a))
      (is (progression/covers? combined browser-request)))))

(deftest browser-cannot-erase-server-requirement-with-empty-wire
  (let [empty-wire
        {:gesso.live.progression/type
         progression/requirement-wire-type
         :gesso.live.progression/version
         progression/progression-version
         :bases []}

        ctx
        {progression.http/canonical-context-key
         (progression/requirement basis-a)

         :headers
         {progression.http/request-header-name
          (encode-wire empty-wire)}}]

    (is (= :invalid-progression-wire
           (error-kind
            #(progression.http/bind-request-progression
              ctx))))))

(deftest invalid-existing-server-requirement-is-not-laundered-by-browser-binding
  (let [browser-b
        (progression/requirement basis-b)

        ctx
        {progression.http/canonical-context-key
         {:forged :server-requirement}

         :headers
         {progression.http/request-header-name
          (progression.http/encode-request-progression
           browser-b)}}]

    (is (= :invalid-requirement
           (error-kind
            #(progression.http/bind-request-progression
              ctx))))))
