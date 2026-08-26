(ns gesso.live.optimistic.server-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.machine :as machine]
   [gesso.live.core :as live]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.server :as server]
   [gesso.live.progression :as progression]))

;; =============================================================================
;; Fixtures / helpers
;; =============================================================================

(def command-id
  (identity/command-id "command-42"))

(def execution-id
  (identity/execution-id "execution-7"))

(def trusted-principal
  (identity/principal "helper-1"))

(def other-principal
  (identity/principal "attacker"))

(def observed-basis
  {:tx-id 42
   :system-time "2026-08-25T01:00:00Z"})

(def authoritative-basis
  {:tx-id 43
   :system-time "2026-08-25T01:00:01Z"})

(def authoritative-request
  (protocol/authoritative
   {:presence :present
    :basis authoritative-basis
    :projection {:request/id "request-1"
                 :request/status :claimed
                 :request/claimed-by "helper-1"}
    :fact-versions {:request/status 10}}))

(defn command-envelope
  ([]
   (command-envelope {}))
  ([overrides]
   (protocol/command
    (merge
     {:command-id command-id
      :execution-id execution-id
      :operation :request/claim
      :arguments {:request-id "request-1"}
      :observed-basis observed-basis
      :scope [:request "request-1"]
      :fact-versions {:request/status 9}}
     overrides))))

(defn confirmed-result
  ([]
   (confirmed-result {}))
  ([overrides]
   (merge
    {:resolution :confirmed
     :authoritative authoritative-request
     :outcome :request/claimed}
    overrides)))

(defn rejected-result
  ([]
   {:resolution :rejected
    :reason :not-authorized})
  ([overrides]
   (merge
    {:resolution :rejected
     :reason :not-authorized}
    overrides)))

(defn error-data
  [f]
  (try
    (f)
    nil
    (catch Throwable ex
      (ex-data ex))))

(defn error-kind
  [f]
  (:error/kind
   (error-data f)))

(defn base-operation
  ([execute!]
   (base-operation execute! {}))
  ([execute! overrides]
   (server/operation
    (merge
     {:name :request/claim-optimistic
      :operation :request/claim
      :execute! execute!}
     overrides))))

(defn base-server
  ([execute!]
   (base-server execute! {}))
  ([execute! overrides]
   (server/server
    (merge
     {:principal-fn (fn [ctx]
                      (:authenticated-principal ctx))
      :operations
      {:request/claim
       (base-operation execute!)}}
     overrides))))

(def trusted-ctx
  {:authenticated-principal trusted-principal
   :browser-principal other-principal
   :request/id "http-request-1"})

;; =============================================================================
;; Registry construction / canonical plans
;; =============================================================================

(deftest operation-construction-builds-one-canonical-authority-plan
  (let [calls (atom 0)
        real-plan optimistic-choreo/command-plan
        operation
        (with-redefs [optimistic-choreo/command-plan
                      (fn [options role]
                        (swap! calls inc)
                        (real-plan options role))]
          (base-operation (fn [_] (confirmed-result))))]
    (is (= 1 @calls))
    (is (server/operation? operation))
    (is (machine/executable-plan? (:authority-plan operation)))
    (is (= :request/claim (:operation operation)))
    (is (= :authority (:authority-role operation)))
    (is (= :browser (:browser-role operation)))))

(deftest request-execution-reuses-precompiled-authority-plan
  (let [calls (atom 0)
        real-plan optimistic-choreo/command-plan
        operation
        (with-redefs [optimistic-choreo/command-plan
                      (fn [options role]
                        (swap! calls inc)
                        (real-plan options role))]
          (base-operation (fn [_] (confirmed-result))))
        prepared-server
        (server/server
         {:principal-fn (fn [ctx] (:authenticated-principal ctx))
          :operations {:request/claim operation}})]
    (is (= 1 @calls))
    (with-redefs [optimistic-choreo/command-plan
                  (fn [& _]
                    (swap! calls inc)
                    (throw (ex-info "request-time recompilation" {})))]
      (let [first-run (server/run-command prepared-server trusted-ctx (command-envelope))
            second-run (server/run-command prepared-server trusted-ctx (command-envelope))]
        (is (server/prepared-send? first-run))
        (is (server/prepared-send? second-run))))
    (is (= 1 @calls))))

(deftest operation-and-registry-configuration-fail-closed
  (is (= :same-role
         (error-kind
          #(base-operation
            (fn [_] (confirmed-result))
            {:browser-role :same
             :authority-role :same}))))
  (is (= :unknown-key
         (error-kind
          #(server/operation
            {:name :request/claim-optimistic
             :operation :request/claim
             :execute! (fn [_] (confirmed-result))
             :browser-can-authorize true}))))
  (is (= :operation-registry-mismatch
         (error-kind
          #(server/server
            {:principal-fn (fn [_] trusted-principal)
             :operations
             {:request/unclaim
              (base-operation (fn [_] (confirmed-result)))}})))))

;; =============================================================================
;; Trusted principal / operation boundary
;; =============================================================================

(deftest principal-is-derived-from-trusted-server-context
  (let [seen (atom nil)
        prepared-server
        (base-server
         (fn [operation-ctx]
           (reset! seen operation-ctx)
           (confirmed-result)))
        prepared
        (server/run-command
         prepared-server
         trusted-ctx
         (command-envelope))]
    (is (= trusted-principal (:principal @seen)))
    (is (not= other-principal (:principal @seen)))
    (is (= trusted-principal (:principal prepared)))
    (is (= :request/claim (:operation @seen)))
    (is (= {:request-id "request-1"}
           (:arguments @seen)))
    (is (= observed-basis (:observed-basis @seen)))
    (is (= [:request "request-1"] (:scope @seen)))
    (is (= {:request/status 9} (:fact-versions @seen)))))

(deftest untyped-principal-is-rejected-before-public-operation-runs
  (let [called? (atom false)
        prepared-server
        (server/server
         {:principal-fn (fn [_] "helper-1")
          :operations
          {:request/claim
           (base-operation
            (fn [_]
              (reset! called? true)
              (confirmed-result)))}})]
    (is (= :invalid-principal
           (error-kind
            #(server/run-command
              prepared-server
              trusted-ctx
              (command-envelope)))))
    (is (false? @called?))))

(deftest browser-cannot-select-unregistered-operation
  (let [called? (atom false)
        prepared-server
        (base-server
         (fn [_]
           (reset! called? true)
           (confirmed-result)))
        malicious-command
        (command-envelope
         {:operation :organization/delete
          :arguments {:organization-id "org-1"}})]
    (is (= :unknown-operation
           (error-kind
            #(server/run-command
              prepared-server
              trusted-ctx
              malicious-command))))
    (is (false? @called?))))

(deftest wire-command-cannot-smuggle-principal-or-authority-fields
  (let [prepared-server
        (base-server (fn [_] (confirmed-result)))
        wire
        (assoc (protocol/command->wire (command-envelope))
               :principal (identity/encode-wire other-principal))]
    (is (= :unknown-fields
           (:error/kind
            (error-data
             #(server/run-wire-command
               prepared-server
               trusted-ctx
               wire)))))))

(deftest begin-command-reaches-only-the-registry-selected-authority-boundary
  (let [prepared-server
        (base-server (fn [_] (confirmed-result)))
        boundary
        (server/begin-command
         prepared-server
         trusted-ctx
         (command-envelope))
        action
        (machine/pending-action (:execution boundary))]
    (is (server/command-boundary? boundary))
    (is (= trusted-principal (:principal boundary)))
    (is (= :request/claim
           (get-in boundary [:operation-entry :operation])))
    (is (= :request/claim (:operation action)))
    (is (machine/waiting-authoritative? (:execution boundary)))))

;; =============================================================================
;; Operation-result / settlement boundary
;; =============================================================================

(deftest operation-result-cannot-smuggle-protocol-correlation-identities
  (let [prepared-server
        (base-server (fn [_] (confirmed-result)))
        boundary
        (server/begin-command prepared-server trusted-ctx (command-envelope))]
    (is (= :unknown-key
           (error-kind
            #(server/settlement-from-result
              boundary
              {:resolution :confirmed
               :authoritative authoritative-request
               :command-id (identity/command-id "forged")}))))
    (is (= :unknown-key
           (error-kind
            #(server/settlement-from-result
              boundary
              {:resolution :confirmed
               :authoritative authoritative-request
               :execution-id (identity/execution-id "forged")}))))))

(deftest settlement-correlation-is-copied-from-the-validated-command
  (let [prepared-server
        (base-server (fn [_] (confirmed-result)))
        boundary
        (server/begin-command prepared-server trusted-ctx (command-envelope))
        settlement
        (server/settlement-from-result boundary (confirmed-result))]
    (is (= command-id (:command-id settlement)))
    (is (= execution-id (:execution-id settlement)))
    (is (= :confirmed (:resolution settlement)))
    (is (= authoritative-request (:authoritative settlement)))
    (is (= :request/claimed (:outcome settlement)))))

(deftest successful-resolutions-require-authoritative-observation
  (let [prepared-server
        (base-server (fn [_] (confirmed-result)))
        boundary
        (server/begin-command prepared-server trusted-ctx (command-envelope))]
    (doseq [resolution [:confirmed :reconciled :already-incorporated]]
      (is (= :missing-settlement-authority
             (:error/kind
              (error-data
               #(server/settlement-from-result
                 boundary
                 {:resolution resolution}))))))))

(deftest rejection-may-avoid-disclosing-authoritative-state
  (let [prepared-server
        (base-server (fn [_] (rejected-result)))
        prepared
        (server/run-command prepared-server trusted-ctx (command-envelope))]
    (is (= :rejected (get-in prepared [:settlement :resolution])))
    (is (= "not-authorized" (get-in prepared [:settlement :reason])))
    (is (not (contains? (:settlement prepared) :authoritative)))))

(deftest trusted-operation-may-explicitly-classify-semantic-failure
  (let [prepared-server
        (base-server
         (fn [_]
           {:resolution :failed
            :reason :pre-commit-validation-failed}))
        prepared
        (server/run-command prepared-server trusted-ctx (command-envelope))]
    (is (= :failed (get-in prepared [:settlement :resolution])))
    (is (= "pre-commit-validation-failed"
           (get-in prepared [:settlement :reason])))
    (is (not (contains? (:settlement prepared) :authoritative)))))

;; =============================================================================
;; Choreo settlement send
;; =============================================================================

(deftest prepared-settlement-send-is-correlated-and-wire-ready
  (let [prepared-server
        (base-server (fn [_] (confirmed-result)))
        prepared
        (server/run-command prepared-server trusted-ctx (command-envelope))]
    (is (server/prepared-send? prepared))
    (is (= trusted-principal (:principal prepared)))
    (is (= :request/claim (:operation prepared)))
    (is (= command-id (get-in prepared [:settlement :command-id])))
    (is (= execution-id (get-in prepared [:settlement :execution-id])))
    (is (= (:settlement prepared)
           (protocol/wire->settlement (:settlement-wire prepared))))
    (is (= command-id (get-in prepared [:payload :command-id])))
    (is (= execution-id (get-in prepared [:payload :execution-id])))
    (is (machine/waiting-send? (:execution prepared)))))

(deftest completing-settlement-send-terminates-the-authority-projection
  (let [prepared-server
        (base-server (fn [_] (confirmed-result)))
        prepared
        (server/run-command prepared-server trusted-ctx (command-envelope))
        completed
        (server/complete-settlement-send prepared)]
    (is (server/completed-send? completed))
    (is (machine/completed? (:execution completed)))
    (is (= (:message prepared) (:message completed)))
    (is (= (:settlement prepared) (:settlement completed)))
    (is (= (:settlement-wire prepared) (:settlement-wire completed)))))

(deftest public-operation-is-invoked-exactly-once-per-command
  (let [calls (atom [])
        prepared-server
        (base-server
         (fn [operation-ctx]
           (swap! calls conj operation-ctx)
           (confirmed-result)))
        prepared
        (server/run-command prepared-server trusted-ctx (command-envelope))]
    (is (server/prepared-send? prepared))
    (is (= 1 (count @calls)))
    (is (= command-id (:command-id (first @calls))))
    (is (= execution-id (:execution-id (first @calls))))))

(deftest run-wire-command-decodes-browser-wire-before-entering-trusted-boundary
  (let [seen (atom nil)
        prepared-server
        (base-server
         (fn [operation-ctx]
           (reset! seen operation-ctx)
           (confirmed-result)))
        wire-command
        (protocol/command->wire (command-envelope))
        prepared
        (server/run-wire-command prepared-server trusted-ctx wire-command)]
    (is (server/prepared-send? prepared))
    (is (= command-id (:command-id @seen)))
    (is (= execution-id (:execution-id @seen)))
    (is (= :request/claim (:operation @seen)))))

;; =============================================================================
;; Post-commit classification / adversarial failures
;; =============================================================================

(deftest arbitrary-operation-exception-is-never-reclassified-as-failed-settlement
  (let [committed? (atom false)
        expected
        (ex-info "post-commit invalidation failed"
                 {:failure/stage :post-commit-delivery})
        prepared-server
        (base-server
         (fn [_]
           ;; This deliberately models the dangerous case: durable authority has
           ;; changed before a later delivery/invalidation step throws.
           (reset! committed? true)
           (throw expected)))]
    (try
      (server/run-command prepared-server trusted-ctx (command-envelope))
      (is false "Expected the original operation exception to escape.")
      (catch Throwable actual
        (is (identical? expected actual))))
    (is (true? @committed?)
        "The trusted boundary must not pretend the committed mutation rolled back.")))

(deftest classified-live-post-commit-failure-escapes-unchanged
  (let [delivery-cause
        (ex-info
         "post-commit invalidation submission failed"
         {:delivery :async})
        system {:options {}}
        tx-ctx {:xtdb/connectable :node}
        operation-called? (atom false)
        prepared-server
        (base-server
         (fn [_]
           (reset! operation-called? true)
           (with-redefs [live/execute-tx!
                         (fn [_ctx _tx-ops _tx-options]
                           {:tx-result {:tx-id 73}
                            :consistency {:after-tx 73}
                            :progression (progression/requirement :basis/commit-73)})

                         live/submit-expanded!
                         (fn [_system _ctx _change _entry]
                           (throw delivery-cause))]
             (live/transact-and-notify!
              system
              tx-ctx
              {:tx-ops [[:synthetic/committed-tx]]
               :change {:topic :request
                        :id "request-1"}}))))
        actual
        (try
          (server/run-command
           prepared-server
           trusted-ctx
           (command-envelope))
          nil
          (catch Throwable error
            error))
        data (ex-data actual)]
    (is (true? @operation-called?))
    (is (live/post-commit-delivery-failure? actual)
        "the optimistic boundary must preserve Live's committed/delivery-failed classification")
    (is (= :committed (:commit/status data)))
    (is (= :post-commit-delivery (:failure/stage data)))
    (is (= :async (:emit data)))
    (is (= 0 (:delivery/index data)))
    (is (= {:topic :request
            :id "request-1"
            :progression (progression/requirement :basis/commit-73)}
           (:delivery/change data)))
    (is (= [] (:delivery/completed-results data)))
    (is (identical? delivery-cause (.getCause ^Throwable actual)))
    (is (nil? (:settlement data))
        "post-commit delivery failure must not be converted into a semantic :failed settlement")))

(deftest malformed-operation-result-fails-without-inventing-settlement
  (let [prepared-server
        (base-server
         (fn [_]
           {:resolution :confirmed
            :authoritative authoritative-request
            :browser-says-authoritative true}))]
    (is (= :unknown-key
           (error-kind
            #(server/run-command
              prepared-server
              trusted-ctx
              (command-envelope)))))))

(deftest incompatible-wire-command-is-rejected-before-trusted-boundary-runs
  (let [principal-called? (atom false)
        operation-called? (atom false)
        prepared-server
        (base-server
         (fn [_]
           (reset! operation-called? true)
           (confirmed-result))
         {:principal-fn
          (fn [_]
            (reset! principal-called? true)
            trusted-principal)})
        wire
        (assoc (protocol/command->wire (command-envelope))
               :protocol-version "999")
        data
        (error-data
         #(server/run-wire-command
           prepared-server
           trusted-ctx
           wire))]
    (is (= :unsupported-version (:error/kind data)))
    (is (= {:status :incompatible
            :supported protocol/version
            :encountered "999"}
           (:protocol/version-status data)))
    (is (false? @principal-called?)
        "Protocol incompatibility must be rejected before trusted principal derivation.")
    (is (false? @operation-called?)
        "Protocol incompatibility must be rejected before public operation execution.")))

(deftest malformed-wire-version-is-not-misclassified-as-incompatible
  (let [principal-called? (atom false)
        operation-called? (atom false)
        prepared-server
        (base-server
         (fn [_]
           (reset! operation-called? true)
           (confirmed-result))
         {:principal-fn
          (fn [_]
            (reset! principal-called? true)
            trusted-principal)})
        wire
        (assoc (protocol/command->wire (command-envelope))
               :protocol-version 999)
        data
        (error-data
         #(server/run-wire-command
           prepared-server
           trusted-ctx
           wire))]
    (is (= :invalid-protocol-version (:error/kind data)))
    (is (= :invalid
           (get-in data [:protocol/version-status :status])))
    (is (= :malformed-version
           (get-in data [:protocol/version-status :reason])))
    (is (false? @principal-called?))
    (is (false? @operation-called?))))

(deftest malformed-future-looking-wire-command-remains-structural-error
  (let [principal-called? (atom false)
        operation-called? (atom false)
        prepared-server
        (base-server
         (fn [_]
           (reset! operation-called? true)
           (confirmed-result))
         {:principal-fn
          (fn [_]
            (reset! principal-called? true)
            trusted-principal)})
        wire
        (assoc (protocol/command->wire (command-envelope))
               :protocol-version "999"
               :forged-authority :authority)
        data
        (error-data
         #(server/run-wire-command
           prepared-server
           trusted-ctx
           wire))]
    (is (= :unknown-fields (:error/kind data))
        "A version-looking malformed envelope is not a recognizable stale protocol command.")
    (is (nil? (:protocol/version-status data))
        "Structural rejection happens before protocol-version recovery classification.")
    (is (false? @principal-called?))
    (is (false? @operation-called?))))
