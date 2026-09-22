(ns gesso.choreo.type-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.knowledge :as knowledge]
   [gesso.choreo.type :as type]))

(defn- error-kind
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (:error/kind
       (ex-data ex)))))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (ex-data ex))))

(def command-id
  (identity/command-id
   "command-1"))

(def execution-id
  (identity/execution-id
   "execution-1"))

(deftest schema-registry-exposes-the-portable-vocabulary
  (is
   (= #{::type/role
        ::type/principal
        ::type/actor
        ::type/host
        ::type/authority
        ::type/authority-name
        ::type/command-id
        ::type/execution-id
        ::type/trust-class
        ::type/fact-key
        ::type/provenance
        ::type/basis
        ::type/outcome
        ::type/fact
        ::type/authoritative-basis-progression
        ::type/command
        ::type/provisional-value
        ::type/compatibility-identity
        ::type/compatibility-set}
      (set
       (keys
        type/schemas))))

  (doseq [schema-key
          (keys type/schemas)]
    (is
     (some?
      (type/schema
       schema-key)))

    (is
     (fn?
      (type/validator
       schema-key)))))

(deftest unknown-schema-keys-fail-closed
  (is
   (= :unknown-schema
      (error-kind
       #(type/schema
         ::missing))))

  (is
   (= :unknown-schema
      (error-kind
       #(type/validator
         ::missing))))

  (is
   (= :unknown-schema
      (error-kind
       #(type/valid?
         ::missing
         :anything))))

  (let [data
        (error-data
         #(type/schema
           ::missing))]
    (is
     (= ::missing
        (:schema-key data)))

    (is
     (= (set (keys type/schemas))
        (:known-schema-keys data)))))

(deftest role-authority-name-fact-key-outcome-and-trust-class-are-keyword-shaped
  (doseq [[schema-key value]
          [[::type/role :browser]
           [::type/authority-name :request-model]
           [::type/fact-key :request/status]
           [::type/outcome :confirmed]
           [::type/trust-class :trusted]
           [::type/trust-class :application/custom-trust-class]]]
    (is
     (type/valid?
      schema-key
      value)))

  (doseq [[schema-key value]
          [[::type/role "browser"]
           [::type/authority-name "request-model"]
           [::type/fact-key "request/status"]
           [::type/outcome "confirmed"]
           [::type/trust-class "trusted"]]]
    (is
     (false?
      (type/valid?
       schema-key
       value))))

  (is
   (= #{:trusted :untrusted}
      type/built-in-trust-classes)))

(deftest runtime-identity-types-remain-distinct
  (let [raw
        "same-raw-value"

        principal
        (identity/principal raw)

        actor
        (identity/actor raw)

        host
        (identity/host raw)

        authority
        (identity/authority raw)

        command
        (identity/command-id raw)

        execution
        (identity/execution-id raw)]

    (is (type/valid? ::type/principal principal))
    (is (type/valid? ::type/actor actor))
    (is (type/valid? ::type/host host))
    (is (type/valid? ::type/authority authority))
    (is (type/valid? ::type/command-id command))
    (is (type/valid? ::type/execution-id execution))

    (is (false? (type/valid? ::type/principal actor)))
    (is (false? (type/valid? ::type/actor principal)))
    (is (false? (type/valid? ::type/host authority)))
    (is (false? (type/valid? ::type/authority host)))
    (is (false? (type/valid? ::type/command-id execution)))
    (is (false? (type/valid? ::type/execution-id command)))))

(deftest identity-shape-validation-does-not-claim-authentication
  (let [browser-claim
        (identity/principal
         "browser-supplied-principal")]

    ;; The type layer intentionally accepts a correctly shaped identity value.
    ;; Whether this principal is authenticated or authorized belongs to a
    ;; trusted server boundary, not Malli shape validation.
    (is
     (type/valid?
      ::type/principal
      browser-claim))))

(deftest basis-is-opaque-but-must-be-present
  (doseq [basis
          [42
           :xtdb/basis
           "opaque-basis"
           {:tx-id 9001}
           [:authority :revision 12]]]
    (is
     (type/valid?
      ::type/basis
      basis)))

  (is
   (false?
    (type/valid?
     ::type/basis
     nil))))

(deftest provenance-schema-delegates-to-the-canonical-knowledge-shape
  (doseq [provenance
          [(knowledge/input-provenance
            :execution-entry)

           (knowledge/communicated-provenance
            :browser
            :claim-request)

           (knowledge/authoritative-provenance
            :request/claim)

           (knowledge/authoritative-observation-provenance
            :request-model
            :request/read
            {:tx-id 42})

           (knowledge/derived-provenance
            :request/visible?
            #{:request/status})

           (knowledge/asserted-provenance
            :trusted-router)]]
    (is
     (type/valid?
      ::type/provenance
      provenance)))

  (is
   (false?
    (type/valid?
     ::type/provenance
     {:kind :authoritative
      :operation "not-a-keyword"})))

  (is
   (false?
    (type/valid?
     ::type/provenance
     {:kind :invented
      :source :browser}))))

(deftest fact-requires-value-and-at-least-one-valid-provenance
  (let [fact
        {:value :open
         :provenance
         [(knowledge/input-provenance
           :execution-entry)]}]

    (is
     (type/valid?
      ::type/fact
      fact))

    (is
     (type/valid?
      ::type/fact
      (assoc fact :value nil)))

    (is
     (false?
      (type/valid?
       ::type/fact
       {:value :open
        :provenance []})))

    (is
     (false?
      (type/valid?
       ::type/fact
       {:value :open
        :provenance
        [{:kind :invented}]})))

    (is
     (false?
      (type/valid?
       ::type/fact
       (assoc fact :unexpected true))))))

(deftest authoritative-basis-progression-is-shape-only-and-closed
  (let [progression
        {:kind :authoritative-basis-progression
         :authority :request-model
         :observation :request/read
         :from-basis {:tx-id 41}
         :to-basis {:tx-id 42}
         :relation :advances}]

    (is
     (knowledge/authoritative-basis-progression?
      progression))

    (is
     (type/valid?
      ::type/authoritative-basis-progression
      progression))

    ;; Shape validation deliberately cannot establish that 42 really advances
    ;; 41. The authority/consistency layer owns truthfulness of the witness.
    (is
     (type/valid?
      ::type/authoritative-basis-progression
      (assoc progression
             :from-basis {:tx-id 999}
             :to-basis {:tx-id 1})))

    (is
     (false?
      (type/valid?
       ::type/authoritative-basis-progression
       (assoc progression
              :relation :invented-relation))))

    (is
     (false?
      (type/valid?
       ::type/authoritative-basis-progression
       (assoc progression
              :extra true))))))

(deftest command-envelope-preserves-command-identity-and-optional-basis
  (let [without-basis
        {:command-id command-id
         :operation :request/claim
         :arguments {:request-id "request-1"}}

        with-basis
        (assoc without-basis
               :observed-basis
               {:tx-id 41})]

    (is
     (= without-basis
        (type/command
         without-basis)))

    (is
     (= with-basis
        (type/command
         with-basis)))

    (is
     (type/valid?
      ::type/command
      without-basis))

    (is
     (type/valid?
      ::type/command
      with-basis))

    (is
     (false?
      (type/valid?
       ::type/command
       (assoc without-basis
              :command-id execution-id))))

    (is
     (false?
      (type/valid?
       ::type/command
       (assoc without-basis
              :operation "request/claim"))))

    (is
     (false?
      (type/valid?
       ::type/command
       (assoc without-basis
              :arguments [:request-id "request-1"]))))

    (is
     (false?
      (type/valid?
       ::type/command
       (assoc without-basis
              :observed-basis nil))))

    (is
     (false?
      (type/valid?
       ::type/command
       (assoc without-basis
              :execution-id execution-id))))))

(deftest command-envelope-does-not-treat-arguments-as-authorization
  (let [forged-looking-command
        {:command-id command-id
         :operation :admin/delete-everything
         :arguments {:principal "someone-else"
                     :claims {:role :admin
                              :authorized? true}}
         :observed-basis {:browser-claims-revision 999999}}]

    ;; The portable envelope is intentionally only structural. Trusted server
    ;; projection/auth/model boundaries must decide whether this operation and
    ;; these arguments are legal for the authenticated principal.
    (is
     (type/valid?
      ::type/command
      forged-looking-command))))

(deftest provisional-values-require-command-and-authoritative-basis-context
  (let [value
        {:authority :provisional
         :command-id command-id
         :observed-basis {:tx-id 41}
         :projection {:status :claimed
                      :claimed-by "helper-1"}}]

    (is
     (= value
        (type/provisional-value
         value)))

    (is
     (type/valid?
      ::type/provisional-value
      value))

    (is
     (false?
      (type/valid?
       ::type/provisional-value
       (dissoc value
               :command-id))))

    (is
     (false?
      (type/valid?
       ::type/provisional-value
       (dissoc value
               :observed-basis))))

    (is
     (false?
      (type/valid?
       ::type/provisional-value
       (assoc value
              :authority :authoritative))))

    (is
     (false?
      (type/valid?
       ::type/provisional-value
       (assoc value
              :command-id execution-id))))

    (is
     (false?
      (type/valid?
       ::type/provisional-value
       (assoc value
              :observed-basis nil))))

    (is
     (false?
      (type/valid?
       ::type/provisional-value
       (assoc value
              :extra :not-allowed))))))

(deftest compatibility-identities-keep-dimensions-independent
  (let [choreography
        (type/compatibility-identity
         :choreography
         4)

        executable
        (type/compatibility-identity
         :executable-plan
         "sha256:plan")

        wire
        (type/compatibility-identity
         :wire-protocol
         3)]

    (is
     (= {:kind :choreography
         :value 4}
        choreography))

    (is
     (= #{choreography
          executable
          wire}
        (type/compatibility-set
         #{choreography
           executable
           wire})))

    (is
     (= #{:choreography
          :executable-plan
          :semantic-operation
          :wire-protocol
          :deployment}
        type/built-in-compatibility-kinds))

    (is
     (type/valid?
      ::type/compatibility-identity
      {:kind :application/request-schema
       :value 7})))
    )

(deftest compatibility-set-rejects-two-values-for-one-dimension
  (let [left
        {:kind :wire-protocol
         :value 2}

        right
        {:kind :wire-protocol
         :value 3}]

    (is
     (false?
      (type/valid?
       ::type/compatibility-set
       #{left right})))

    (is
     (= :invalid-value
        (error-kind
         #(type/compatibility-set
           #{left right})))))
    )

(deftest compatibility-values-must-be-present-and-identities-are-closed
  (is
   (false?
    (type/valid?
     ::type/compatibility-identity
     {:kind :wire-protocol
      :value nil})))

  (is
   (false?
    (type/valid?
     ::type/compatibility-identity
     {:kind :wire-protocol
      :value 3
      :extra :not-allowed})))

  (is
   (false?
    (type/valid?
     ::type/compatibility-set
     [{:kind :wire-protocol
       :value 3}]))))

(deftest validate-returns-input-and-invalid-values-carry-malli-explanation
  (let [value
        {:command-id command-id
         :operation :request/claim
         :arguments {:request-id "request-1"}}]

    (is
     (= value
        (type/validate!
         ::type/command
         value)))

    (is
     (nil?
      (type/explain-data
       ::type/command
       value)))

    (let [data
          (error-data
           #(type/validate!
             ::type/command
             (assoc value
                    :command-id execution-id)))]

      (is
       (= :invalid-value
          (:error/kind data)))

      (is
       (= ::type/command
          (:schema-key data)))

      (is
       (= execution-id
          (get-in data
                  [:value :command-id])))

      (is
       (some?
        (:explanation data))))))
