(ns gesso.live.optimistic.protocol-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (ex-data ex))))

(defn- error-kind
  [f]
  (:error/kind
   (error-data f)))

(def command-id
  (identity/command-id
   "command-42"))

(def execution-id
  (identity/execution-id
   "execution-7"))

(def retry-execution-id
  (identity/execution-id
   "execution-8"))

(def basis
  {:tx-id 42
   :system-time "2026-08-25T01:00:00Z"})

(def newer-basis
  {:tx-id 43
   :system-time "2026-08-25T01:00:01Z"})

(def fact-versions
  {:request/status 9
   :request/claim "claim-v3"})

(defn- command-envelope
  ([]
   (command-envelope {}))
  ([overrides]
   (protocol/command
    (merge
     {:command-id command-id
      :execution-id execution-id
      :operation :request/claim
      :arguments {:request-id "request-1"}
      :observed-basis basis
      :scope [:request "request-1"]
      :fact-versions fact-versions}
     overrides))))

(defn- provisional-envelope
  ([]
   (provisional-envelope {}))
  ([overrides]
   (protocol/provisional
    (merge
     {:command-id command-id
      :execution-id execution-id
      :observed-basis basis
      :projection {:request/status :claimed
                   :request/claimed-by "helper-1"}
      :scope [:request "request-1"]
      :fact-versions fact-versions}
     overrides))))

(defn- present-authority
  ([]
   (present-authority {}))
  ([overrides]
   (protocol/authoritative
    (merge
     {:presence :present
      :basis newer-basis
      :projection {:request/status :claimed
                   :request/claimed-by "helper-1"}
      :fact-versions {:request/status 10}}
     overrides))))

(defn- absent-authority
  ([]
   (protocol/authoritative
    {:presence :absent
     :basis newer-basis}))
  ([overrides]
   (protocol/authoritative
    (merge
     {:presence :absent
      :basis newer-basis}
     overrides))))

(deftest protocol-v3-vocabulary-is-explicit-and-closed
  (is (= "3" protocol/version))
  (is (= :optimistic/command protocol/command-event))
  (is (= :optimistic/provisional protocol/provisional-event))
  (is (= :optimistic/settlement protocol/settlement-event))
  (is (= :optimistic/authoritative protocol/authoritative-event))

  (is (= #{:present :absent}
         protocol/authoritative-presences))
  (is (= #{:confirmed
           :reconciled
           :rejected
           :already-incorporated
           :failed}
         protocol/settlement-resolutions))
  (is (= #{:confirmed
           :reconciled
           :already-incorporated}
         protocol/authoritative-required-resolutions))
  (is (= (conj protocol/settlement-resolutions :superseded)
         protocol/provisional-resolution-kinds))

  (is (= #{:command-id :execution-id}
         protocol/command-correlation-keys))
  (is (= #{:command-id :execution-id}
         protocol/settlement-correlation-keys)))

(deftest semantic-names-remain-portable-without-becoming-authority
  (is (= "request/claim"
         (protocol/qualified-name :request/claim)))
  (is (= "request/claim"
         (protocol/qualified-name 'request/claim)))
  (is (= "42"
         (protocol/qualified-name 42)))
  (is (nil? (protocol/qualified-name nil)))

  (is (= "request/claim"
         (protocol/normalize-name :operation :request/claim)))
  (is (= "conflict"
         (protocol/normalize-optional-name :reason :conflict)))
  (is (nil? (protocol/normalize-optional-name :reason nil)))
  (is (= :invalid-name
         (error-kind
          #(protocol/normalize-name :operation "   ")))))

(deftest command-and-execution-identities-remain-distinct
  (let [raw "same-raw-value"
        command (identity/command-id raw)
        execution (identity/execution-id raw)]
    (is (not= command execution))
    (is (identity/same-raw-value? command execution))
    (is (= command
           (protocol/wire->command-id
            (protocol/command-id->wire command))))
    (is (= execution
           (protocol/wire->execution-id
            (protocol/execution-id->wire execution))))
    (is (= :invalid-command-id
           (error-kind
            #(protocol/require-command-id execution))))
    (is (= :invalid-execution-id
           (error-kind
            #(protocol/require-execution-id command))))
    (is (= :invalid-command-id
           (error-kind
            #(protocol/wire->command-id
              (protocol/execution-id->wire execution)))))
    (is (= :invalid-execution-id
           (error-kind
            #(protocol/wire->execution-id
              (protocol/command-id->wire command)))))))

(deftest command-construction-keeps-semantic-and-protocol-identities-separate
  (let [command (command-envelope)]
    (is (= protocol/version
           (:protocol-version command)))
    (is (= command-id
           (:command-id command)))
    (is (= execution-id
           (:execution-id command)))
    (is (= :request/claim
           (:operation command)))
    (is (= {:request-id "request-1"}
           (:arguments command)))
    (is (= basis
           (:observed-basis command)))
    (is (= [:request "request-1"]
           (:scope command)))
    (is (= fact-versions
           (:fact-versions command))))

  (testing "a semantic command can be retried with a new execution identity"
    (let [retry (command-envelope
                 {:execution-id retry-execution-id})]
      (is (= command-id (:command-id retry)))
      (is (= retry-execution-id (:execution-id retry)))))

  (testing "commands need not be optimistic and may omit observed basis"
    (let [command (protocol/command
                   {:command-id command-id
                    :execution-id execution-id
                    :operation :request/read
                    :arguments {:request-id "request-1"}})]
      (is (not (contains? command :observed-basis)))))

  (testing "the constructor is closed"
    (is (= :unknown-fields
           (error-kind
            #(protocol/command
              {:command-id command-id
               :execution-id execution-id
               :operation :request/claim
               :arguments {}
               :browser-authority true}))))))

(deftest command-shape-does-not-coerce-operation-or-arguments
  (is (= :gesso.choreo.type/error
         (:error/type
          (error-data
           #(protocol/command
             {:command-id command-id
              :execution-id execution-id
              :operation "request/claim"
              :arguments {}})))))
  (is (= :gesso.choreo.type/error
         (:error/type
          (error-data
           #(protocol/command
             {:command-id command-id
              :execution-id execution-id
              :operation :request/claim
              :arguments [:request-id "request-1"]}))))))

(deftest scope-remains-ordinary-portable-correlation-data
  (doseq [scope [[:request "request-1"]
                 {:request-id "request-1"}
                 :request/all
                 "request-1"]]
    (is (= scope
           (protocol/normalize-scope scope)))
    (is (= scope
           (protocol/wire-scope scope))))

  (is (nil? (protocol/normalize-scope nil)))
  (is (= :invalid-scope
         (error-kind
          #(protocol/normalize-scope "   ")))))

(deftest fact-versions-remain-model-owned-and-distinct-from-basis
  (is (= fact-versions
         (protocol/normalize-fact-versions fact-versions)))
  (is (nil? (protocol/normalize-fact-versions nil)))
  (is (= :invalid-fact-versions
         (error-kind
          #(protocol/normalize-fact-versions [:request/status 9]))))
  (is (= :invalid-fact-version-key
         (error-kind
          #(protocol/normalize-fact-versions {"request/status" 9}))))
  (is (= :invalid-fact-version
         (error-kind
          #(protocol/normalize-fact-versions {:request/status nil})))))

(deftest bases-are-required-when-authority-or-provisionality-depends-on-them
  (is (= basis
         (protocol/normalize-basis :basis basis)))
  (is (nil? (protocol/normalize-optional-basis :basis nil)))
  (is (= :missing-basis
         (error-kind
          #(protocol/normalize-basis :basis nil))))

  (is (= :missing-basis
         (error-kind
          #(protocol/provisional
            {:command-id command-id
             :execution-id execution-id
             :observed-basis nil
             :projection {}}))))
  (is (= :missing-basis
         (error-kind
          #(protocol/authoritative
            {:presence :present
             :basis nil
             :projection {}})))))

(deftest provisional-values-are-explicitly-provisional
  (let [provisional (provisional-envelope)]
    (is (= protocol/version
           (:protocol-version provisional)))
    (is (= :provisional
           (:authority provisional)))
    (is (= command-id
           (:command-id provisional)))
    (is (= execution-id
           (:execution-id provisional)))
    (is (= basis
           (:observed-basis provisional)))
    (is (= {:request/status :claimed
            :request/claimed-by "helper-1"}
           (:projection provisional))))

  (testing "callers cannot supply an authority field to the constructor"
    (is (= :unknown-fields
           (error-kind
            #(protocol/provisional
              {:authority :authoritative
               :command-id command-id
               :execution-id execution-id
               :observed-basis basis
               :projection {}}))))))

(deftest command-and-provisional-values-must-correlate
  (let [{command' :command
         provisional' :provisional}
        (protocol/command-provisional-pair
         (command-envelope)
         (provisional-envelope))]
    (is (= command-id (:command-id command')))
    (is (= execution-id (:execution-id provisional')))
    (is (= basis (:observed-basis command')))
    (is (= basis (:observed-basis provisional'))))

  (is (= :correlation-mismatch
         (error-kind
          #(protocol/command-provisional-pair
            (command-envelope)
            (provisional-envelope
             {:execution-id retry-execution-id})))))

  (is (= :basis-mismatch
         (error-kind
          #(protocol/command-provisional-pair
            (command-envelope)
            (provisional-envelope
             {:observed-basis newer-basis})))))

  (is (= :command-missing-observed-basis
         (error-kind
          #(protocol/command-provisional-pair
            (protocol/command
             {:command-id command-id
              :execution-id execution-id
              :operation :request/claim
              :arguments {}})
            (provisional-envelope))))))

(deftest authoritative-presence-and-absence-are-explicit
  (let [present (present-authority)
        absent (absent-authority)]
    (is (= :authoritative (:authority present)))
    (is (= :present (:presence present)))
    (is (= newer-basis (:basis present)))
    (is (contains? present :projection))

    (is (= :authoritative (:authority absent)))
    (is (= :absent (:presence absent)))
    (is (= newer-basis (:basis absent)))
    (is (not (contains? absent :projection))))

  (testing "present nil is distinct from authoritative absence"
    (let [present-nil (protocol/authoritative
                       {:presence :present
                        :basis newer-basis
                        :projection nil})]
      (is (contains? present-nil :projection))
      (is (nil? (:projection present-nil)))))

  (is (= :missing-authoritative-projection
         (error-kind
          #(protocol/authoritative
            {:presence :present
             :basis newer-basis}))))
  (is (= :projection-on-authoritative-absence
         (error-kind
          #(absent-authority {:projection nil}))))
  (is (= :invalid-authoritative-presence
         (error-kind
          #(protocol/authoritative
            {:presence :unknown
             :basis newer-basis}))))
  (is (= :unknown-fields
         (error-kind
          #(protocol/authoritative
            {:presence :absent
             :basis newer-basis
             :command-applied? false})))))

(deftest successful-settlements-require-authority
  (doseq [resolution protocol/authoritative-required-resolutions]
    (let [settlement (protocol/settlement
                      {:command-id command-id
                       :execution-id execution-id
                       :resolution resolution
                       :authoritative (present-authority)
                       :outcome :request/claimed})]
      (is (= resolution (:resolution settlement)))
      (is (= command-id (:command-id settlement)))
      (is (= execution-id (:execution-id settlement)))
      (is (= :authoritative
             (get-in settlement [:authoritative :authority]))))

    (is (= :missing-settlement-authority
           (error-kind
            #(protocol/settlement
              {:command-id command-id
               :execution-id execution-id
               :resolution resolution})))))

  (testing "rejection may avoid disclosing protected authority"
    (is (= :rejected
           (:resolution
            (protocol/settlement
             {:command-id command-id
              :execution-id execution-id
              :resolution :rejected
              :reason :unauthorized})))))

  (testing "trusted failure may omit authority but is not command-applied metadata"
    (is (= :failed
           (:resolution
            (protocol/settlement
             {:command-id command-id
              :execution-id execution-id
              :resolution :failed
              :reason :operation-failed})))))

  (testing "supersession is learned from authority, not forged as a settlement"
    (is (contains? protocol/provisional-resolution-kinds :superseded))
    (is (not (contains? protocol/settlement-resolutions :superseded)))
    (is (= :invalid-settlement-resolution
           (error-kind
            #(protocol/settlement
              {:command-id command-id
               :execution-id execution-id
               :resolution :superseded})))))

  (testing "application outcome remains distinct from generic resolution"
    (let [settlement (protocol/settlement
                      {:command-id command-id
                       :execution-id execution-id
                       :resolution :confirmed
                       :authoritative (present-authority)
                       :outcome :request/claimed
                       :reason :accepted})]
      (is (= :confirmed (:resolution settlement)))
      (is (= :request/claimed (:outcome settlement)))
      (is (= "accepted" (:reason settlement)))))

  (is (= :invalid-settlement-outcome
         (error-kind
          #(protocol/settlement
            {:command-id command-id
             :execution-id execution-id
             :resolution :rejected
             :outcome "request/claimed"})))))

(deftest command-wire-round-trip-preserves-v3-values
  (let [runtime (command-envelope)
        wire (protocol/command->wire runtime)
        decoded (protocol/wire->command wire)]
    (is (= runtime decoded))
    (is (= protocol/version (:protocol-version wire)))
    (is (= :command
           (:gesso.choreo.identity.wire/kind
            (:command-id wire))))
    (is (= :execution
           (:gesso.choreo.identity.wire/kind
            (:execution-id wire))))
    (is (= [:request "request-1"]
           (:scope wire)))
    (is (= basis (:observed-basis wire))))

  (testing "wrong identity kind remains rejected after transport"
    (let [wire (protocol/command->wire (command-envelope))]
      (is (= :invalid-command-id
             (error-kind
              #(protocol/wire->command
                (assoc wire
                       :command-id
                       (protocol/execution-id->wire execution-id))))))))

  (testing "wire envelope is versioned and closed"
    (let [wire (protocol/command->wire (command-envelope))]
      (is (= :unsupported-version
             (error-kind
              #(protocol/wire->command
                (assoc wire :protocol-version "4")))))
      (is (= :unknown-fields
             (error-kind
              #(protocol/wire->command
                (assoc wire :principal "browser-claim"))))))))

(deftest provisional-wire-round-trip-preserves-explicit-provisionality
  (let [runtime (provisional-envelope)
        wire (protocol/provisional->wire runtime)]
    (is (= runtime
           (protocol/wire->provisional wire)))
    (is (= :provisional (:authority wire))))

  (let [wire (protocol/provisional->wire (provisional-envelope))]
    (is (= :invalid-provisional-authority
           (error-kind
            #(protocol/wire->provisional
              (assoc wire :authority :authoritative)))))))

(deftest authoritative-wire-round-trip-preserves-tombstones
  (doseq [runtime [(present-authority)
                   (absent-authority)]]
    (is (= runtime
           (protocol/wire->authoritative
            (protocol/authoritative->wire runtime)))))

  (let [wire (protocol/authoritative->wire (absent-authority))]
    (is (= :invalid-authoritative-authority
           (error-kind
            #(protocol/wire->authoritative
              (assoc wire :authority :provisional)))))))

(deftest settlement-wire-round-trip-preserves-correlation-and-authority
  (doseq [settlement
          [(protocol/settlement
            {:command-id command-id
             :execution-id execution-id
             :resolution :confirmed
             :authoritative (present-authority)
             :outcome :request/claimed})

           (protocol/settlement
            {:command-id command-id
             :execution-id execution-id
             :resolution :reconciled
             :authoritative (absent-authority)})

           (protocol/settlement
            {:command-id command-id
             :execution-id retry-execution-id
             :resolution :already-incorporated
             :authoritative (present-authority)})

           (protocol/settlement
            {:command-id command-id
             :execution-id execution-id
             :resolution :rejected
             :reason :unauthorized})

           (protocol/settlement
            {:command-id command-id
             :execution-id execution-id
             :resolution :failed
             :reason :operation-failed})]]
    (is (= settlement
           (protocol/wire->settlement
            (protocol/settlement->wire settlement)))))

  (testing "the settlement wire remains closed"
    (let [wire (protocol/settlement->wire
                (protocol/settlement
                 {:command-id command-id
                  :execution-id execution-id
                  :resolution :rejected}))]
      (is (= :unknown-fields
             (error-kind
              #(protocol/wire->settlement
                (assoc wire :role :server))))))))

(deftest v2-fields-are-not-accepted-by-v3-constructors
  (doseq [[constructor envelope removed-key removed-value]
          [[protocol/command
            {:command-id command-id
             :execution-id execution-id
             :operation :request/claim
             :arguments {}}
            :transition
            :claim]

           [protocol/provisional
            {:command-id command-id
             :execution-id execution-id
             :observed-basis basis
             :projection {}}
            :projection-mode
            :provisional]

           [protocol/settlement
            {:command-id command-id
             :execution-id execution-id
             :resolution :rejected}
            :command-applied?
            false]]]
    (is (= :unknown-fields
           (error-kind
            #(constructor
              (assoc envelope removed-key removed-value)))))))
