(ns gesso.assurance.proof.observed-serial-history-correspondence
  "v1403: Compare a rechecked v1402 ABSTRACT serialization witness with
   separately supplied v1392 XTDB submission/ledger diagnostic reports.

   Both inputs are caller-transferrable Clojure data. A v1392 report records a
   process-local adapter invocation and a later query on a caller-provided
   node; it does not authenticate a payload, causal attempt, or physical order.
   A matching reported tx-id ordering is therefore only a BOUNDED DIAGNOSTIC.
   The comparison never runs a command, queries XTDB, authorizes retries,
   settles optimistic UI, or issues a certificate. Its source contains private
   abstract bindings; use redacted-summary for presentation."
  (:require [gesso.assurance.proof.abstract-serial-history-witness :as witness]
            [gesso.assurance.proof.xtdb-submission-outcome-correlation :as submission]
            [gesso.model.command-resolution :as resolution])
  (:import [java.time Instant]))

(def report-type
  :gesso.assurance.proof.observed-serial-history-correspondence/report)
(def report-version 1)
(def max-observations witness/max-events)

(def assurance-scope
  {:checked #{:replay-of-supplied-abstract-history
              :bounded-identity-and-commit-observation-comparison
              :reported-transaction-id-order-versus-abstract-order
              :missing-duplicate-and-contradictory-observation-detection
              :redacted-recheckable-diagnostic}
   :assumed #{:faithful-abstract-events
              :faithful-independent-observation-inputs
              :v1392-process-local-reports-not-substituted}
   :not-established #{:physical-transaction-order
                      :report-origin-authentication
                      :actual-submitted-payload-and-attempt-attribution
                      :complete-writer-coverage
                      :transaction-internal-prestate-or-authorization
                      :physical-serializability-or-refinement
                      :retry-or-settlement-authority
                      :application-certification}})

(def ^:private physical-open-obligations
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :actual-inputs-and-authorization-to-issued-executor-binding
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})

(def ^:private source-keys #{:abstract-witness :observations})
(def ^:private observation-keys
  #{:attempt-id :command-id :submission-diagnostic})
(def ^:private report-keys
  #{:type :version :source :status :discrepancies :comparison
    :observation-origins :physical-order-authenticated?
    :attempt-to-physical-commit-authenticated? :physical-refinement
    :safe-to-retry? :safe-to-settle? :authority :certificate
    :proof-status :open-obligations})

(def ^:private original-witness-check witness/require-current-witness!)

(defn- reject! [reason]
  (throw (ex-info "Invalid observed serial-history comparison."
                  {:observed-serial-history/rejection true :reason reason})))

(defn- exact-map? [m ks]
  (and (map? m) (= ks (set (keys m)))))

(defn- valid-diagnostic? [d]
  ;; This validates a copied diagnostic's shape and internally consistent
  ;; no-authority markings; it CANNOT authenticate where it came from.
  (and (map? d)
       (= submission/report-type (:type d))
       (= submission/report-version (:version d))
       (= :process-local-adapter-and-caller-node (:observation-origin d))
       (false? (:actual-xtdb-api-authenticated? d))
       (= :not-established (:node-identical-to-submission-connectable d))
       (= :not-established (:transaction-internal-prestate d))
       (= :not-established (:complete-writer-coverage d))
       (= :diagnostic-only (:proof-status d))
       (= :not-issued (:certificate d))
       (= :not-admitted (:admission d))
       (= :none (:authority d))
       (= physical-open-obligations (:open-obligations d))
       (map? (:ledger-observation d))
       (keyword? (:status d))
       (contains? #{:submit-tx :execute-tx nil} (:api d))))

(defn- positive-ledger-report? [d]
  (let [ledger (:ledger-observation d)
        checks (:checks ledger)]
    (and (= :reported-execute-ledger-agrees (:status d))
         (= :execute-tx (:api d))
         (= :returned (:api-completion d))
         (= :returned (:thunk-outcome d))
         (= :reported-key-matches-committed-ledger-row (:status ledger))
         (= #{:tx-id :committed :system-time} (set (keys checks)))
         (every? true? (vals checks))
         (integer? (:reported-tx-id d))
         (<= 0 (:reported-tx-id d) Long/MAX_VALUE)
         (instance? Instant (:reported-system-time d))
         (uuid? (:call-id d)))))

(defn- observation-class [d]
  (cond
    (not (valid-diagnostic? d)) :invalid
    (= :reported-execute-ledger-agrees (:status d))
    (if (positive-ledger-report? d) :reported-ledger-agreement :invalid)
    ;; Never treat a nonmatching ledger or an ID-only submit as a physical
    ;; rejection. Even v1392's :threw status leaves commit unknown.
    :else :unconfirmed))

(defn- issue [reason index]
  ;; Deliberately omit command bindings, attempted payloads and record values.
  {:reason reason :observation-index index})

(defn- analyse [abstract-witness observations]
  (let [history (:final-abstract-history abstract-witness)
        attempts (:attempts history)
        serial (:serial-history abstract-witness)
        incorporated-ids (set (map :attempt-id serial))
        indexes (group-by (comp :attempt-id :observation) (map-indexed
                    (fn [i observation]
                      {:index i :observation observation}) observations))
        ;; Reject unknown and duplicate correlations; don't silently use the
        ;; first entry or let a supplied vector order imply XTDB order.
        mapping-issues
        (vec (mapcat
              (fn [[id rows]]
                (concat
                 (when (> (count rows) 1)
                   [(issue :duplicate-attempt-observation (:index (first rows)))])
                 (when-not (contains? attempts id)
                   [(issue :unrecognized-abstract-attempt (:index (first rows)))])))
              indexes))
        checked
        (mapv (fn [i {:keys [attempt-id command-id submission-diagnostic]}]
                (let [attempt (get attempts attempt-id)
                      expected-id (when attempt
                                    (resolution/record-id (:binding attempt)))
                      cls (observation-class submission-diagnostic)
                      incorporated? (contains? incorporated-ids attempt-id)
                      identity-ok? (and attempt (= expected-id command-id))
                      problems (cond-> []
                                 (not identity-ok?)
                                 (conj :command-identity-disagrees)
                                 (= :invalid cls)
                                 (conj :invalid-submission-diagnostic)
                                 (and (not incorporated?)
                                      (= :reported-ledger-agreement cls))
                                 (conj :positive-report-for-nonincorporation))]
                  {:index i :attempt-id attempt-id
                   :classification cls :problems problems
                   :tx-id (when (= :reported-ledger-agreement cls)
                            (:reported-tx-id submission-diagnostic))}))
              (range) observations)
        by-attempt (into {} (map (juxt :attempt-id identity) checked))
        incorporation-issues
        (vec (mapcat
              (fn [{:keys [attempt-id]}]
                (let [v (get by-attempt attempt-id)]
                  (cond
                    (nil? v) [(issue :missing-incorporation-observation nil)]
                    (not= :reported-ledger-agreement (:classification v))
                    [(issue :incorporation-not-correlated-with-committed-ledger
                            (:index v))]
                    :else []))) serial))
        positive-rows (filterv #(= :reported-ledger-agreement
                                   (:classification %)) checked)
        duplicate-tx-issues
        (vec (for [[_ rows] (group-by :tx-id positive-rows)
                   :when (> (count rows) 1)]
               (issue :duplicate-reported-transaction-id (:index (first rows)))))
        ordered-ids (mapv (fn [{:keys [attempt-id]}]
                            (let [rows (get indexes attempt-id)
                                  entry (when (= 1 (count rows))
                                          (get by-attempt attempt-id))]
                              (when (and entry (empty? (:problems entry))
                                         (= :reported-ledger-agreement
                                            (:classification entry)))
                                (:tx-id entry)))) serial)
        observed-all? (every? integer? ordered-ids)
        order-issue (when (and observed-all?
                               (not (every? (fn [[a b]] (< a b))
                                            (partition 2 1 ordered-ids))))
                      [(issue :reported-tx-order-disagrees-with-abstract-order nil)])
        discrepancies (vec (concat mapping-issues
                                   (mapcat (fn [{:keys [index problems]}]
                                             (map #(issue % index) problems))
                                           checked)
                                   incorporation-issues
                                   duplicate-tx-issues
                                   order-issue))
        missing-count (count (filter #(= :missing-incorporation-observation
                                         (:reason %)) discrepancies))
        matched-count (count (filter integer? ordered-ids))
        status (cond
                 (seq discrepancies) :reported-execution-divergence-or-incomplete
                 (empty? serial) :no-abstract-incorporations
                 :else :bounded-reported-metadata-order-agreement)]
    {:status status
     :discrepancies discrepancies
     :comparison {:abstract-incorporations (count serial)
                  :supplied-execution-observations (count observations)
                  :matched-reported-incorporations matched-count
                  :missing-incorporation-observations missing-count
                  :reported-order-comparison
                  (cond
                    (empty? serial) :not-applicable
                    (not observed-all?) :incomplete
                    (seq order-issue) :divergent
                    :else :strictly-increasing-reported-tx-ids)}}))

(defn- construct! [source]
  (when-not (exact-map? source source-keys)
    (reject! :invalid-comparison-source))
  (let [{:keys [abstract-witness observations]} source]
    (when-not (and (vector? observations)
                   (<= (count observations) max-observations)
                   (every? #(exact-map? % observation-keys) observations))
      (reject! :invalid-observation-envelope))
    (when-not (identical? original-witness-check
                          (var-get #'witness/require-current-witness!))
      (reject! :substituted-abstract-witness-checker))
    (original-witness-check abstract-witness)
    (let [{:keys [status discrepancies comparison]}
          (analyse abstract-witness observations)]
      {:type report-type
       :version report-version
       :source source
       :status status
       :discrepancies discrepancies
       :comparison comparison
       :observation-origins
       {:abstract-order :caller-supplied-rechecked-v1402-events
        :transaction-metadata :caller-supplied-v1392-adapter-and-ledger-diagnostics
        :origin-trust :unattested-copyable-process-local-data}
       :physical-order-authenticated? false
       :attempt-to-physical-commit-authenticated? false
       :physical-refinement :not-established
       :safe-to-retry? false
       :safe-to-settle? false
       :authority :none
       :certificate :not-issued
       :proof-status :reported-metadata-comparison-only
       :open-obligations (:not-established assurance-scope)})))

(defn compare-observations!
  "Compare a rechecked PRIVATE v1402 witness with separately supplied v1392
   submission diagnostics, each associated with a claimed attempt and command
   ID. The association is an UNAUTHENTICATED caller claim. One diagnostic per
   attempt; vector order is not a physical sequencing witness. Missing or
   contradictory observations produce explicit discrepancies, not authority."
  [source]
  (construct! source))

(defn require-current-report!
  "Recreate the entire diagnostic from the embedded source, rejecting stale,
   altered or incomplete fields. Does not authenticate either source."
  [report]
  (when-not (and (exact-map? report report-keys)
                 (= report (construct! (:source report))))
    (reject! :altered-or-stale-observation-report))
  report)

(defn redacted-summary
  "No private command bindings, outcomes, source witnesses or transaction keys."
  [report]
  (require-current-report! report)
  {:type :gesso.assurance.proof.observed-serial-history-correspondence/summary
   :version report-version
   :status (:status report)
   :comparison (:comparison report)
   :discrepancies (:discrepancies report)
   :physical-order-authenticated? false
   :attempt-to-physical-commit-authenticated? false
   :physical-refinement :not-established
   :safe-to-retry? false
   :safe-to-settle? false
   :authority :none
   :certificate :not-issued
   :proof-status :reported-metadata-comparison-only})
