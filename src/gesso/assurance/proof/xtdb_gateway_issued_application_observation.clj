(ns gesso.assurance.proof.xtdb-gateway-issued-application-observation
  "v1867: Process-local, one-attempt-per-certified-operation collection of
   original issued-executor typed candidate diagnostics. Each selected
   operation is actually probed through its own original executor; no caller
   can hand in or merge a copyable 'successful observation' report.

   Observations are independent and potentially occur at different XTDB
   bases. No common snapshot, historical consistency, callback purity,
   deployment adoption, physical alias submission, proof discharge, or
   application certification follows from collecting every operation.

   v1869: Store one-use attempt states and qualified observations in ONE
   atomically updated, per-operation ledger. Concurrent summaries distinguish
   in-flight, failed, and completed probes without mixing consumed-attempt
   and qualified-evidence snapshots. Historical pinned reads are never
   represented as current physical prestate guarantees.

   v1870: A separately requested follow-up read rechecks ONLY the original
   source document/target accessor against a NEW pinned XTDB snapshot. The
   original executor and constructor are never rerun. The result is a later
   point-in-time comparison, NOT equality to the historical candidate,
   continuous freshness or transaction authority.

   v1871: Retain the exact originally checked before-document and target
   ONLY in a private qualified observation entry. The public summary exposes
   two independent later read outcomes: new source document versus physical
   row, and historical checked before-document versus that same physical row.
   Witness contents are never published, replayed, or used for authority.

   v1872: Retain the checked read's private single-point XTDB snapshot basis
   and classify each later pinned read relative to THAT reported database/time
   point. Basis progression is not row-change history or an ABA proof.

   v1873: Also carry the selected row's visible, bounded system-time boundary
   presence, separately from whole-database progression. Absence is not
   evidence of no change; neither status is continuous history certification."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.checked-authorization-assembly :as coverage]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial :as issued]
            [gesso.assurance.proof.xtdb-gateway-issued-application-recheck :as recheck]
            [gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity :as snapshot]
            [gesso.live.application-preflight.certified-contract :as certified]))

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
        original-recheck recheck/compare-original-acquired-prestate!
        states (atom (into (sorted-map)
                           (for [operation (:certified-operation-ids initial)]
                             [operation :not-attempted])))
        recheck! (fn []
                   (when-not (= initial
                                (original-current-scope! contract-source catalog-source))
                     (reject! :original-application-observation-source-changed))
                   true)
        summary! (fn []
                   (recheck!)
                   ;; Take ONE immutable snapshot: attempts, pending work and
                   ;; qualified evidence must never be read from separate
                   ;; mutable structures under concurrent observation.
                   (let [snapshot @states
                         qualified (into (sorted-map)
                                         (for [[op entry] snapshot
                                               :when (map? entry)]
                                           [op (:evidence entry)]))
                         completed (set (keys qualified))
                         in-flight (into #{} (for [[op v] snapshot
                                                   :when (= v :in-flight)] op))
                         failed (into #{} (for [[op v] snapshot
                                                :when (= v :failed)] op))]
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
                          (for [[operation state] snapshot]
                            [operation (get qualified operation :not-observed)]))
                    :per-operation-selected-row-history-boundaries
                    (into (sorted-map)
                          (for [[operation state] snapshot]
                            [operation (if (map? state)
                                         (:selected-row-history-boundary state
                                          :not-checked)
                                         :not-eligible)]))
                    :selected-row-history-meaning
                    :visible-pinned-system-time-boundary-presence-only
                    :selected-row-history-completeness :not-established
                    :selected-row-history-proves-no-aba? false
                    :per-operation-reported-basis-relations
                    (into (sorted-map)
                          (for [[operation state] snapshot]
                            [operation (if (map? state)
                                         (:reported-basis-relation state :not-checked)
                                         :not-eligible)]))
                    :basis-comparison-scope
                    :reported-same-database-system-time-not-row-change-history
                    :basis-observation-proves-no-aba? false
                    :per-operation-historical-prestate-rechecks
                    (into (sorted-map)
                          (for [[operation state] snapshot]
                            [operation (if (map? state)
                                         (:historical-prestate-recheck
                                          state :not-checked)
                                         :not-eligible)]))
                    :historical-prestate-recheck-meaning
                    :original-checked-before-against-later-pinned-row
                    :per-operation-read-only-rechecks
                    (into (sorted-map)
                          (for [[operation state] snapshot]
                            [operation (if (map? state)
                                         (:read-only-recheck state :not-checked)
                                         :not-eligible)]))
                    :read-only-recheck-meaning
                    :later-source-accessor-against-new-pinned-read-not-historical-candidate
                    :read-only-recheck-transaction-authority :none
                    :per-operation-attempt-status
                    (into (sorted-map)
                          (for [[operation state] snapshot]
                            [operation (if (map? state) :qualified state)]))
                    :observed-operation-ids completed
                    :in-flight-observation-ids in-flight
                    :failed-observation-ids failed
                    :attempted-without-qualified-evidence
                    (set/union in-flight failed)
                    :observation-count (count qualified)
                    :all-certified-operations-individually-observed?
                    (= (:certified-operation-ids initial) completed)
                    :physical-prestate-currentness :not-established
                    :physical-observation-temporality
                    :historical-individual-pinned-reads
                    :physical-state-rechecked-at-summary? false
                    :source-stability :original-source-rechecked-at-summary
                    :joint-snapshot-basis :not-established
                    :physical-application-correspondence :not-established
                    :deployed-handler-adoption :not-established
                    :callback-purity :not-established
                    :original-kernel-proof-discharge :not-established
                    :physical-refinement-proof-discharge :not-established
                    :certificate :not-issued :admission :not-admitted
                    :transaction-authority :none :retry-authority :none
                    :authority :none}))
        observe! (fn [operation ctx invocation]
                   (when-not (contains? (:certified-operation-ids initial)
                                        operation)
                     (reject! :operation-not-originally-certified))
                   ;; swap-vals! reserves one permit and records :in-flight in
                   ;; the SAME atomic ledger. There is no window where the
                   ;; permit is consumed but a summary says not attempted.
                   (let [[prior _] (swap-vals!
                                    states
                                    (fn [current]
                                      (if (= :not-attempted
                                             (get current operation))
                                        (assoc current operation :in-flight)
                                        current)))]
                     (when-not (= :not-attempted (get prior operation))
                       (reject! :issued-application-observation-already-attempted)))
                   (try
                     (recheck!)
                     (let [captured-before (volatile! nil)
                           result (original-observe
                                   contract-source catalog-source operation
                                   ctx invocation nil :pinned-source-derived-premises
                                   (fn [witness] (vreset! captured-before witness)))
                           checked (qualified-observation! initial operation result)
                           original-before @captured-before]
                       (when-not (and (map? original-before)
                                      (= #{:operation-id :target :before
                                           :original-snapshot-point :source-identity}
                                         (set (keys original-before)))
                                      (= operation (:operation-id original-before))
                                      (map? (:before original-before))
                                      (let [p (:original-snapshot-point original-before)]
                                        (or (nil? p)
                                            (snapshot/valid-private-point? p)))
                                      (= (:selected-operation-source-identity result)
                                         (:source-identity original-before)))
                         (reject! :original-private-prestate-witness-not-established))
                       (recheck!)
                       (swap! states assoc operation
                              {:evidence checked
                               :private-original-before original-before
                               :original-context ctx
                               :original-invocation invocation
                               :read-only-recheck :not-checked
                               :historical-prestate-recheck :not-checked
                               :reported-basis-relation :not-checked
                               :selected-row-history-boundary :not-checked})
                       (summary!))
                     (catch Throwable failure
                       ;; Never convert an unsuccessful/ambiguous attempt
                       ;; into evidence. A completed probe remains consumed.
                       ;; Caller callbacks may already have caused effects.
                       (swap! states
                              (fn [current]
                                (if (= :in-flight (get current operation))
                                  (assoc current operation :failed)
                                  current)))
                       (throw failure))))
        read-only-recheck! (fn [operation]
                             (when-not (contains? (:certified-operation-ids initial)
                                                  operation)
                               (reject! :operation-not-originally-certified))
                             (recheck!)
                             (let [[prior _] (swap-vals!
                                              states
                                              (fn [current]
                                                (let [entry (get current operation)]
                                                  (if (and (map? entry)
                                                           (not= :in-flight
                                                                 (:read-only-recheck entry)))
                                                    (assoc-in current [operation :read-only-recheck]
                                                              :in-flight)
                                                    current))))
                                   entry (get prior operation)]
                               (when-not (map? entry)
                                 (reject! :original-observation-required-for-read-only-recheck))
                               (when (= :in-flight (:read-only-recheck entry))
                                 (reject! :read-only-recheck-already-in-flight))
                               (try
                                 (when-not (identical? original-recheck
                                                       (var-get #'recheck/compare-original-acquired-prestate!))
                                   (reject! :substituted-original-read-only-recheck-function))
                                 (let [source (get-in contract-source
                                                      [:certified-operations operation :current-source])
                                       executor (get-in contract-source
                                                        [:certified-operations operation :executor])
                                       report (original-recheck
                                               source executor operation
                                               (:original-context entry)
                                               (:original-invocation entry)
                                               (:private-original-before entry))]
                                   (when-not (and (= recheck/report-type (:type report))
                                                  (= recheck/report-version (:version report))
                                                  (= operation (:operation-id report))
                                                  (#{:source-document-matched-at-read
                                                     :source-document-differed-at-read}
                                                   (:comparison report))
                                                  (#{:historical-prestate-matched-at-read
                                                     :historical-prestate-differed-at-read}
                                                   (:historical-prestate-at-read report))
                                                  (= :same-original-execution-target
                                                     (:historical-target report))
                                                  (#{:not-comparable
                                                     :different-reported-databases
                                                     :earlier-reported-database-basis
                                                     :later-reported-database-basis
                                                     :same-reported-database-basis}
                                                   (:reported-basis-relation report))
                                                  (#{:visible-selected-row-system-boundary-observed
                                                     :no-visible-selected-row-system-boundary-returned
                                                     :same-reported-basis-no-interval
                                                     :not-comparable}
                                                   (:selected-row-system-history-boundary report))
                                                  (= :not-established
                                                     (:selected-row-history-completeness report))
                                                  (= false
                                                     (:selected-row-history-proves-no-aba? report))
                                                  (= :not-established
                                                     (:historical-candidate-equality report)
                                                     (:currentness-after-read report))
                                                  (= :none (:transaction-authority report)
                                                     (:authority report)))
                                     (reject! :unqualified-original-read-only-recheck))
                                   (recheck!)
                                   (swap! states update operation
                                          (fn [e] (assoc e
                                                         :read-only-recheck
                                                         (:comparison report)
                                                         :historical-prestate-recheck
                                                         (:historical-prestate-at-read report)
                                                         :reported-basis-relation
                                                         (:reported-basis-relation report)
                                                         :selected-row-history-boundary
                                                         (:selected-row-system-history-boundary report))))
                                   (summary!))
                                 (catch Throwable failure
                                   (swap! states update operation
                                          (fn [e] (assoc e
                                                         :read-only-recheck :failed
                                                         :historical-prestate-recheck :failed
                                                         :reported-basis-relation :failed
                                                         :selected-row-history-boundary :failed)))
                                   (throw failure)))))]
    {:type report-type :version report-version
     :observe! observe! :summary! summary!
     :read-only-recheck! read-only-recheck!}))
