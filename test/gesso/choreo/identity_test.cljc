(ns gesso.choreo.identity-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]))

(defn- error-kind
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (:error/kind
       (ex-data ex)))))

(deftest roles-remain-static-protocol-keywords
  (is
   (identity/role?
    :browser))

  (is
   (identity/role?
    :server))

  (is
   (false?
    (identity/role?
     "browser")))

  (is
   (= :browser
      (identity/require-role
       :browser)))

  (is
   (= :invalid-role
      (error-kind
       #(identity/require-role
         "browser")))))

(deftest specific-identity-constructors-preserve-kind-and-raw-value
  (let [principal
        (identity/principal
         "user-1")

        actor
        (identity/actor
         "helper-1")

        authority
        (identity/authority
         :request-model)

        host
        (identity/host
         "aleph-2")

        command
        (identity/command-id
         #uuid "00000000-0000-0000-0000-000000000001")

        execution
        (identity/execution-id
         #uuid "00000000-0000-0000-0000-000000000002")]

    (doseq [value
            [principal
             actor
             authority
             host
             command
             execution]]

      (is
       (identity/identity?
        value)))

    (is
     (= :principal
        (identity/kind
         principal)))

    (is
     (= "user-1"
        (identity/raw-value
         principal)))

    (is
     (identity/principal?
      principal))

    (is
     (identity/actor?
      actor))

    (is
     (identity/authority?
      authority))

    (is
     (identity/host?
      host))

    (is
     (identity/command-id?
      command))

    (is
     (identity/execution-id?
      execution))))

(deftest same-raw-value-does-not-collapse-different-identity-kinds
  (let [raw
        "42"

        principal
        (identity/principal raw)

        actor
        (identity/actor raw)

        authority
        (identity/authority raw)

        host
        (identity/host raw)

        command
        (identity/command-id raw)

        execution
        (identity/execution-id raw)

        all
        [principal
         actor
         authority
         host
         command
         execution]]

    (is
     (= 6
        (count
         (set all))))

    (doseq [left all
            right all
            :when (not=
                   (identity/kind left)
                   (identity/kind right))]

      (is
       (not=
        left
        right))

      (is
       (identity/same-raw-value?
        left
        right))

      (is
       (false?
        (identity/same-kind?
         left
         right))))))

(deftest same-kind-and-raw-value-have-normal-value-equality
  (let [left
        (identity/principal
         "user-1")

        right
        (identity/principal
         "user-1")]

    (is
     (= left
        right))

    (is
     (identity/same-kind?
      left
      right))

    (is
     (identity/same-raw-value?
      left
      right))))

(deftest command-id-and-execution-id-are-distinct-even-when-raw-id-matches
  (let [raw
        #uuid "00000000-0000-0000-0000-000000000042"

        command
        (identity/command-id raw)

        execution
        (identity/execution-id raw)]

    (is
     (identity/command-id?
      command))

    (is
     (identity/execution-id?
      execution))

    (is
     (not=
      command
      execution))

    (is
     (identity/same-raw-value?
      command
      execution))

    (is
     (false?
      (identity/same-kind?
       command
       execution)))))

(deftest identity-raw-value-may-be-opaque-portable-edn
  (let [raw
        {:tenant :store/id-17
         :user-id 123
         :revision 9}

        principal
        (identity/principal raw)]

    (is
     (= raw
        (identity/raw-value
         principal)))

    (is
     (identity/principal?
      principal))))

(deftest wire-codec-has-an-explicit-versioned-shape
  (let [command
        (identity/command-id
         "command-1")]

    (is
     (= {:gesso.choreo.identity.wire/type
         :gesso.choreo.identity/wire
         :gesso.choreo.identity.wire/version
         1
         :gesso.choreo.identity.wire/kind
         :command
         :gesso.choreo.identity.wire/value
         "command-1"}
        (identity/encode-wire
         command)))

    (is
     (= 1
        identity/wire-version))

    (is
     (= :gesso.choreo.identity/wire
        identity/wire-type))))

(deftest wire-round-trip-preserves-every-runtime-identity-kind
  (let [raw
        "same-raw-value"

        identities
        [(identity/principal raw)
         (identity/actor raw)
         (identity/authority raw)
         (identity/host raw)
         (identity/command-id raw)
         (identity/execution-id raw)]]

    (doseq [value identities]
      (let [encoded
            (identity/encode-wire value)

            decoded
            (identity/decode-wire encoded)]

        (is
         (identity/wire-identity?
          encoded))

        (is
         (= value
            decoded))

        (is
         (= (identity/kind value)
            (identity/kind decoded)))

        (is
         (= raw
            (identity/raw-value decoded)))))

    (testing "same raw command and execution ids remain distinct after crossing the wire boundary"
      (let [command
            (-> (identity/command-id raw)
                identity/encode-wire
                identity/decode-wire)

            execution
            (-> (identity/execution-id raw)
                identity/encode-wire
                identity/decode-wire)]

        (is
         (identity/command-id?
          command))

        (is
         (identity/execution-id?
          execution))

        (is
         (not=
          command
          execution))

        (is
         (identity/same-raw-value?
          command
          execution))))))

(deftest wire-round-trip-preserves-opaque-portable-edn-raw-values
  (let [raw
        {:tenant :store/id-17
         :subject ["user" 123]
         :revision {:basis 9}}

        original
        (identity/principal raw)

        encoded
        (identity/encode-wire original)

        decoded
        (identity/decode-wire encoded)]

    (is
     (= raw
        (:gesso.choreo.identity.wire/value
         encoded)))

    (is
     (= original
        decoded))))

(deftest wire-encoder-rejects-values-that-are-not-runtime-identities
  (doseq [value
          [nil
           :browser
           "command-1"
           {:kind :command
            :value "command-1"}]]

    (is
     (= :invalid-wire-identity
        (error-kind
         #(identity/encode-wire
           value))))))

(deftest wire-decoder-rejects-malformed-or-incompatible-wire-values
  (let [valid
        {:gesso.choreo.identity.wire/type
         :gesso.choreo.identity/wire
         :gesso.choreo.identity.wire/version
         1
         :gesso.choreo.identity.wire/kind
         :command
         :gesso.choreo.identity.wire/value
         "command-1"}]

    (testing "missing or additional fields are not silently accepted"
      (is
       (= :invalid-wire-identity
          (error-kind
           #(identity/decode-wire
             (dissoc
              valid
              :gesso.choreo.identity.wire/value)))))

      (is
       (= :invalid-wire-identity
          (error-kind
           #(identity/decode-wire
             (assoc
              valid
              :unexpected
              true))))))

    (testing "wire type and version are explicit compatibility gates"
      (is
       (= :invalid-wire-identity
          (error-kind
           #(identity/decode-wire
             (assoc
              valid
              :gesso.choreo.identity.wire/type
              :other/wire)))))

      (is
       (= :unsupported-wire-version
          (error-kind
           #(identity/decode-wire
             (assoc
              valid
              :gesso.choreo.identity.wire/version
              2))))))

    (testing "unknown kinds and nil raw values remain invalid identities"
      (is
       (= :invalid-kind
          (error-kind
           #(identity/decode-wire
             (assoc
              valid
              :gesso.choreo.identity.wire/kind
              :session)))))

      (is
       (= :invalid-value
          (error-kind
           #(identity/decode-wire
             (assoc
              valid
              :gesso.choreo.identity.wire/value
              nil))))))))

(deftest wire-identity-predicate-is-total-and-version-sensitive
  (let [valid
        {:gesso.choreo.identity.wire/type
         :gesso.choreo.identity/wire
         :gesso.choreo.identity.wire/version
         1
         :gesso.choreo.identity.wire/kind
         :execution
         :gesso.choreo.identity.wire/value
         "execution-1"}]

    (is
     (identity/wire-identity?
      valid))

    (doseq [value
            [nil
             :execution
             {}
             (dissoc
              valid
              :gesso.choreo.identity.wire/kind)
             (assoc
              valid
              :gesso.choreo.identity.wire/version
              2)
             (assoc
              valid
              :gesso.choreo.identity.wire/kind
              :session)
             (assoc
              valid
              :extra
              true)]]

      (is
       (false?
        (identity/wire-identity?
         value))))))

(deftest nil-cannot-be-an-identity-value
  (doseq [constructor
          [identity/principal
           identity/actor
           identity/authority
           identity/host
           identity/command-id
           identity/execution-id]]

    (is
     (= :invalid-value
        (error-kind
         #(constructor nil))))))

(deftest identity-cannot-be-retagged-as-another-identity
  (let [principal
        (identity/principal
         "user-1")]

    (is
     (= :nested-identity
        (error-kind
         #(identity/actor
           principal))))

    (is
     (= :nested-identity
        (error-kind
         #(identity/identity
           :execution
           principal))))))

(deftest generic-constructor-rejects-unknown-kind
  (is
   (= :invalid-kind
      (error-kind
       #(identity/identity
         :session
         "session-1")))))

(deftest generic-inspection-is-total-on-nonidentities
  (doseq [value
          [nil
           :browser
           "user-1"
           42
           {}
           {:gesso.choreo.identity/type
            identity/identity-type}
           {:gesso.choreo.identity/type
            identity/identity-type
            :gesso.choreo.identity/kind
            :principal
            :gesso.choreo.identity/value
            nil}]]

    (is
     (false?
      (identity/identity?
       value)))

    (is
     (nil?
      (identity/kind
       value)))

    (is
     (nil?
      (identity/raw-value
       value)))))

(deftest kind-predicates-do-not-cross-match
  (let [principal
        (identity/principal
         "same")

        actor
        (identity/actor
         "same")]

    (is
     (identity/principal?
      principal))

    (is
     (false?
      (identity/actor?
       principal)))

    (is
     (identity/actor?
      actor))

    (is
     (false?
      (identity/principal?
       actor)))

    (is
     (false?
      (identity/kind?
       :not-a-kind
       principal)))))

(deftest bindings-are-sparse-and-do-not-infer-relationships
  (let [principal
        (identity/principal
         "user-1")

        execution
        (identity/execution-id
         "execution-1")

        bindings
        (identity/bindings
         {:role :server
          :principal principal
          :execution-id execution})]

    (is
     (= {:role :server
         :principal principal
         :execution-id execution}
        bindings))

    (is
     (nil?
      (:actor bindings)))

    (is
     (nil?
      (:authority bindings)))

    (is
     (nil?
      (:host bindings)))

    (is
     (nil?
      (:command-id bindings)))

    (is
     (identity/bindings?
      bindings))))

(deftest empty-bindings-are-valid
  (is
   (= {}
      (identity/bindings
       {})))

  (is
   (identity/bindings?
    {})))

(deftest bindings-reject-unknown-context-keys
  (is
   (= :unknown-binding
      (error-kind
       #(identity/bindings
         {:role :server
          :request-id "request-1"}))))

  (is
   (false?
    (identity/bindings?
     {:role :server
      :request-id "request-1"}))))

(deftest each-binding-requires-the-correct-identity-kind
  (let [principal
        (identity/principal "1")

        actor
        (identity/actor "1")

        authority
        (identity/authority "1")

        host
        (identity/host "1")

        command
        (identity/command-id "1")

        execution
        (identity/execution-id "1")]

    (doseq [[key wrong]
            [[:principal actor]
             [:actor principal]
             [:authority host]
             [:host authority]
             [:command-id execution]
             [:execution-id command]]]

      (is
       (= :invalid-binding
          (error-kind
           #(identity/bindings
             {key wrong})))))

    (is
     (= {:principal principal
         :actor actor
         :authority authority
         :host host
         :command-id command
         :execution-id execution}
        (identity/bindings
         {:principal principal
          :actor actor
          :authority authority
          :host host
          :command-id command
          :execution-id execution})))))

(deftest role-binding-remains-a-role-not-a-host-or-principal
  (is
   (= :invalid-role
      (error-kind
       #(identity/bindings
         {:role
          (identity/host
           :browser)}))))

  (is
   (= :invalid-role
      (error-kind
       #(identity/bindings
         {:role
          (identity/principal
           :browser)}))))

  (is
   (= {:role :browser}
      (identity/bindings
       {:role :browser}))))

(deftest binding-lookup-validates-the-whole-binding-map
  (let [principal
        (identity/principal
         "user-1")

        bindings
        {:role :server
         :principal principal}]

    (is
     (= :server
        (identity/binding-value
         bindings
         :role)))

    (is
     (= principal
        (identity/binding-value
         bindings
         :principal)))

    (is
     (nil?
      (identity/binding-value
       bindings
       :actor)))

    (is
     (= :unknown-binding
        (error-kind
         #(identity/binding-value
           bindings
           :session-id))))

    (is
     (= :invalid-binding
        (error-kind
         #(identity/binding-value
           {:principal
            (identity/actor "wrong-kind")}
           :principal))))))

(deftest bindings-predicate-is-total
  (doseq [value
          [nil
           []
           :server
           {:principal "user-1"}
           {:role "server"}
           {:unknown true}]]

    (is
     (false?
      (identity/bindings?
       value)))))

(deftest explain-preserves-kind-distinctions
  (let [bindings
        {:role :server
         :principal
         (identity/principal
          "user-1")
         :actor
         (identity/actor
          "helper-1")
         :authority
         (identity/authority
          :request-model)
         :host
         (identity/host
          "aleph-2")
         :command-id
         (identity/command-id
          "command-7")
         :execution-id
         (identity/execution-id
          "execution-11")}]

    (is
     (= {:role :server
         :principal
         {:kind :principal
          :value "user-1"}
         :actor
         {:kind :actor
          :value "helper-1"}
         :authority
         {:kind :authority
          :value :request-model}
         :host
         {:kind :host
          :value "aleph-2"}
         :command-id
         {:kind :command
          :value "command-7"}
         :execution-id
         {:kind :execution
          :value "execution-11"}}
        (identity/explain
         bindings)))))

(deftest identity-reference-does-not-claim-authentication-or-authorization
  (let [principal
        (identity/principal
         "browser-supplied-user-id")

        actor
        (identity/actor
         "browser-supplied-helper-id")

        authority
        (identity/authority
         :request-model)]

    ;; These constructors only create typed identities. No relationship,
    ;; authentication, authorization, or authority grant is inferred.
    (is
     (identity/principal?
      principal))

    (is
     (identity/actor?
      actor))

    (is
     (identity/authority?
      authority))

    (is
     (not=
      principal
      actor))

    (is
     (not=
      actor
      authority))))
