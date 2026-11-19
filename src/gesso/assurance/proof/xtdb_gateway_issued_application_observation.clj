(ns gesso.assurance.proof.xtdb-gateway-issued-application-observation
  "v1867: Process-local, one-attempt-per-certified-operation collection of
   original issued-executor typed candidate diagnostics. Each selected
   operation is actually probed through its own original executor; no caller
   can hand in or merge a copyable 'successful observation' report.

   Observations are independent and potentially occur at different XTDB
   bases. No common snapshot, historical consistency, callback purity,
   deployment adoption, physical alias submission, proof discharge, or
   application certification follows from collecting every operation."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.checked-authorization-assembly :as coverage]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial :as issued]
            [gesso.live.application-preflight.certified-contract :as certified])
  (:import [java.util.concurrent.atomic AtomicBoolean]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-issued-application-observation/collection)
(def report-version 1)

(def ^:private original-contract-check certified/check-certified-contracts!)
(def ^:private original-catalog-check operation-set/check-operation-set!)
(def ^:private original-coverage-check coverage/check-declared-route-coverage!)
(def ^:private original-observe
  issued/check-current-issued-executor-typed-command-alias!)

(defn- reject! [reason]
  (throw (ex-info "Original issued application observations not established."
                  {:reason reason})))

(defn- original-checkers! []
  (when-not (and (identical? original-contract-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-catalog-check
                             (var-get #'operation-set/check-operation-set!))
                 (identical? original-coverage-check
                             (var-get #'coverage/check-declared-route-coverage!))
                 (identical? original-observe
                             (var-get #'issued/check-current-issued-executor-typed-command-alias!)))
    (reject! :substituted-original-application-observation-checker)))

(def ^:private origin-keys
  [:proof-fingerprint :proof-source-fingerprint :proof-subject-fingerprint
   :dependency-graph-fingerprint :transition-fingerprint :route-ids])

(defn- original-current-scope! [contract-source catalog-source]
  (original-checkers!)
  (let [catalog (original-catalog-check (:catalog catalog-source))
        contracts (original-contract-check contract-source)
        closure (original-coverage-check {:contract-source contract-source
                                          :catalog-source catalog-source})
        ids (:catalog-operations closure)
        rows (:certified-operations contracts)
        origins (into (sorted-map)
                      (for [[operation row] rows]
                        [operation (select-keys row origin-keys)]))
        routes (:matched-certified-route-ids closure)
        other-routes (:uncertified-application-route-ids closure)
        other-operations (:uncertified-application-operations closure)
        evidence (select-keys closure
                              [:open-assumptions :open-premises :open-obligations
                               :certified-operation-open-evidence])]
    (when-not (and (= operation-set/report-type (:type catalog))
                   (= :checked (:exact-declared-operation-coverage catalog))
                   (= certified/report-type (:type contracts))
                   (= coverage/coverage-report-type (:type closure))
                   (= :source-relative-checked (:current-assembly-closure closure))
                   (= :not-established (:installed-handler-adoption closure))
                   (= :not-issued (:certificate closure))
                   (= :none (:transaction-authority closure))
                   (set? ids) (seq ids)
                   (= ids (set (keys rows)) (set (keys origins))
                      (:declared-operation-ids catalog))
                   (every? (fn [[operation origin]]
                             (and (qualified-keyword? operation)
                                  (= (set origin-keys) (set (keys origin)))
                                  (every? some? (vals origin))
                                  (set? (:route-ids origin))
                                  (seq (:route-ids origin))))
                           origins)
                   (every? set? ((juxt :open-assumptions :open-premises
                                       :open-obligations) evidence))
                   (map? (:certified-operation-open-evidence evidence))
                   (= ids (set (keys (:certified-operation-open-evidence evidence))))
                   (every? set? [routes other-routes other-operations])
                   (empty? (set/intersection routes other-routes))
                   (empty? (set/intersection ids other-operations))
                   (= (:route-ids contracts) (set/union routes other-routes))
                   (= (:uncertified-operations contracts) other-operations)
                   (= (:open-assumptions contracts) (:open-assumptions evidence))
                   (= (:open-premises contracts) (:open-premises evidence))
                   (= (:open-obligations contracts) (:open-obligations evidence)))
      (reject! :incomplete-original-application-observation-scope))
    {:catalog-fingerprint (:fingerprint catalog)
     :certified-operation-proof-origins origins
     :certified-operation-ids ids
     :certified-route-ids routes
     :uncertified-operation-ids other-operations
     :uncertified-route-ids other-routes
     :original-open-proof-evidence evidence}))

(defn- qualified-observation! [scope operation observed]
  (let [origin (get-in scope [:certified-operation-proof-origins operation])
        row (get-in observed [:certified-operation-observations operation])
        others (disj (:certified-operation-ids scope) operation)
        not-observed {:candidate :not-observed
                      :typed-transition :not-observed
                      :physical-before :not-observed}
        expected-row {:candidate :original-executor-guarded-candidate-checked
                      :typed-transition :original-and-independent-source-relative
                      :physical-before :pinned-same-node-document-match}
        expected-source {:catalog-fingerprint (:catalog-fingerprint scope)
                         :transition-fingerprint (:transition-fingerprint origin)
                         :proof-fingerprint (:proof-fingerprint origin)
                         :route-ids (:route-ids origin)}]
    (when-not (and (= issued/issued-executor-candidate-alias-report-type
                      (:type observed))
                   (= issued/issued-executor-candidate-alias-report-version
                      (:version observed))
                   (= operation (:operation-id observed))
                   (= :original-checked-catalog-derived
                      (:typed-read-premise-provenance observed))
                   (= :original-issued-operation-id
                      (:typed-read-premise-subject observed))
                   (= :one-original-issued-operation-only
                      (:observation-attribution observed))
                   (= :original-and-independent-source-relative
                      (:typed-abstract-alias-correspondence observed))
                   (= :pinned-same-node-document-match (:external-before observed))
                   (= expected-row row (:selected-operation-observation observed))
                   (= others (:other-certified-operations-unobserved observed))
                   (= (:certified-operation-ids scope)
                      (set (keys (:certified-operation-observations observed))))
                   (every? #(= not-observed
                               (get-in observed [:certified-operation-observations %]))
                           others)
                   (= (:certified-operation-proof-origins scope)
                      (:catalog-certified-operation-proof-origins observed))
                   (= expected-source (:selected-operation-source-identity observed))
                   (= (select-keys origin
                                   [:proof-source-fingerprint
                                    :proof-subject-fingerprint
                                    :dependency-graph-fingerprint])
                      (:selected-operation-proof-source-identity observed))
                   (= (:original-open-proof-evidence scope)
                      (:catalog-open-evidence observed))
                   (= (:uncertified-operation-ids scope)
                      (:catalog-uncertified-application-operations observed))
                   (= (:uncertified-route-ids scope)
                      (:catalog-uncertified-application-route-ids observed))
                   (= :none (:uncertified-proof-origins observed)
                      (:uncertified-operation-observations observed)
                      (:uncertified-route-observations observed))
                   (= :retained-open-not-discharged
                      (:proof-obligations-disposition observed))
                   (= :not-established (:whole-catalog-physical-observation observed)
                      (:original-kernel-proof-discharge observed)
                      (:physical-refinement-proof-discharge observed)
                      (:installed-handler-execution observed))
                   (= :blocked-before-canonical-writer (:physical-submission observed))
                   (= :disabled (:canonical-physical-alias-execution observed))
                   (= :not-issued (:certificate observed))
                   (= :not-admitted (:admission observed))
                   (= :none (:authority observed) (:transaction-authority observed)))
      (reject! :original-selected-operation-observation-not-qualified))
    {:candidate :original-executor-guarded-candidate-checked
     :typed-transition :original-and-independent-source-relative
     :physical-before :individually-pinned-prestate-matched
     :proof-origin origin}))

(defn issue-current-issued-application-observer!
  "Issue a process-local, one-attempt-per-operation collector. Only its own
   :observe! can record an observation: the caller supplies operation, ctx
   and canonical invocation, not a report or candidate. Every successful
   attempt runs the original executor with source-derived premises and a
   separate pinned XTDB read. Original application source is rechecked at
   every entry/exit. Failed attempts remain consumed, never retried here.

   Application callbacks may have effects before capture. Independent reads
   may describe different database states; even a full ledger cannot certify
   atomic application-wide behavior or production adoption."
  [contract-source catalog-source]
  (let [initial (original-current-scope! contract-source catalog-source)
        permits (into {} (for [operation (:certified-operation-ids initial)]
                           [operation (AtomicBoolean. false)]))
        observations (atom {})
        consumed (atom #{})
        recheck! (fn []
                   (when-not (= initial
                                (original-current-scope! contract-source catalog-source))
                     (reject! :original-application-observation-source-changed))
                   true)
        summary! (fn []
                   (recheck!)
                   {:type report-type :version report-version
                    :scope :process-local-original-issued-executor-observations
                    :certified-operation-proof-origins
                    (:certified-operation-proof-origins initial)
                    :certified-operation-ids (:certified-operation-ids initial)
                    :uncertified-operation-ids (:uncertified-operation-ids initial)
                    :uncertified-route-ids (:uncertified-route-ids initial)
                    :original-open-proof-evidence (:original-open-proof-evidence initial)
                    :per-operation-observations
                    (into (sorted-map)
                          (for [operation (:certified-operation-ids initial)]
                            [operation (get @observations operation :not-observed)]))
                    :observed-operation-ids (set (keys @observations))
                    :attempted-without-qualified-evidence
                    (set/difference @consumed (set (keys @observations)))
                    :observation-count (count @observations)
                    :all-certified-operations-individually-observed?
                    (= (:certified-operation-ids initial)
                       (set (keys @observations)))
                    :joint-snapshot-basis :not-established
                    :physical-application-correspondence :not-established
                    :deployed-handler-adoption :not-established
                    :callback-purity :not-established
                    :original-kernel-proof-discharge :not-established
                    :physical-refinement-proof-discharge :not-established
                    :certificate :not-issued :admission :not-admitted
                    :transaction-authority :none :retry-authority :none
                    :authority :none})
        observe! (fn [operation ctx invocation]
                   (let [permit (get permits operation)]
                     (when-not permit
                       (reject! :operation-not-originally-certified))
                     (when-not (.compareAndSet permit false true)
                       (reject! :issued-application-observation-already-attempted))
                     (swap! consumed conj operation)
                     (recheck!)
                     (let [result (original-observe
                                   contract-source catalog-source operation
                                   ctx invocation nil :pinned-source-derived-premises)
                           checked (qualified-observation! initial operation result)]
                       (recheck!)
                       (swap! observations assoc operation checked)
                       (summary!))))]
    {:type report-type :version report-version
     :observe! observe! :summary! summary!}))
