(ns gesso.assurance.proof.conditional-single-node-refinement-evidence
  "v1410: Compose bounded, caller-supplied *single-node* diagnostic evidence.

   Require cross-consistency of (1) the independently replayed abstract
   serial-history witness and reported adapter/ledger metadata (v1403),
   (2) the durable command record and authoritative progression diagnostic
   for each modeled incorporation (v1408), and (3) their command, attempt,
   reported transaction ID, and reported system-time correlations.

   This does NOT acquire evidence, run XTDB, authenticate an attempt-to-commit
   relation, prove transaction isolation or physical refinement, or issue an
   application certificate. All source inputs remain caller supplied and
   may be fabricated together. In particular the v1408 attempt association
   is another caller claim, NOT a physical attempt provenance witness.

   Source maps contain private bindings, records and outcomes: do NOT log
   them. Outputs contain only redacted statuses, counts and vector indices.
   Do not use a positive diagnostic to retry, settle, authorize, or certify."
  (:require [gesso.assurance.proof.committed-incorporation-progression
             :as incorporation]
            [gesso.assurance.proof.observed-serial-history-correspondence
             :as serial]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.model.command-resolution :as command]))

(def report-type
  :gesso.assurance.proof.conditional-single-node-refinement-evidence/report)
(def report-version 1)
(def max-incorporations 256)

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

(def assurance-scope
  {:checked #{:abstract-history-replayed-by-v1402
              :submitted-transaction-metadata-compared-by-v1403
              :reported-durable-incorporation-and-progression-compared-by-v1408
              :attempt-command-identity-joins-across-reports
              :reported-transaction-id-and-system-time-join-across-reports
              :missing-duplicate-and-unexpected-incorporation-cases-detected
              :redacted-status-and-count-only-report}
   :assumed #{:caller-supplied-source-is-faithful
              :atomic-command-ledger-and-domain-mutation-in-abstract-model
              :app-provided-executor-guard-and-typed-input-links
              :reported-transaction-metadata-origin-is-accurate
              :command-record-readback-origin-and-basis-are-accurate
              :all-contributing-operations-executed-on-one-node}
   :not-established physical-open-obligations})

(def ^:private source-keys #{:serial-source :incorporation-cases})
(def ^:private case-keys #{:attempt-id :command-id :incorporation-source})
(def ^:private original-serial serial/compare-observations!)
(def ^:private original-incorporation incorporation/compare-incorporation!)
(def ^:private original-command-id command/record-id)
(def ^:private original-public-key xtdb/tx-result-consistency)

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentHashMap x)
           (instance? clojure.lang.PersistentArrayMap x))
       (nil? (meta x))))

(defn- exact-map? [x expected]
  (and (plain-map? x) (= expected (set (keys x)))))

(defn- reject! [reason]
  (throw (ex-info "Invalid conditional single-node evidence source."
                  {:conditional-single-node-evidence/rejection true
                   :reason reason})))

(defn- issue [reason index]
  ;; Numeric case index only; never include private command or attempt IDs.
  {:reason reason :case-index index})

(defn- source-check! [source]
  (when-not (and (exact-map? source source-keys)
                 (exact-map? (:serial-source source)
                             #{:abstract-witness :observations})
                 (vector? (:incorporation-cases source))
                 (<= (count (:incorporation-cases source))
                     max-incorporations)
                 (every? #(exact-map? % case-keys)
                         (:incorporation-cases source)))
    (reject! :invalid-source-shape))
  ;; Do not accept substituted in-process validators; this is not a tamper-
  ;; resistant process boundary or evidence provenance authentication.
  (when-not (and (identical? original-serial
                             (var-get #'serial/compare-observations!))
                 (identical? original-incorporation
                             (var-get #'incorporation/compare-incorporation!))
                 (identical? original-command-id
                             (var-get #'command/record-id))
                 (identical? original-public-key
                             (var-get #'xtdb/tx-result-consistency)))
    (reject! :substituted-upstream-checker))
  source)

(defn- private-receipt-key [incorporation-source]
  (try
    (let [resolution (get-in incorporation-source
                             [:observation :execution :result
                              :controlled-result :resolution])
          transaction (:transaction resolution)]
      (when (and (= :incorporated (:status resolution))
                 (map? transaction))
        (original-public-key (:tx-result transaction))))
    (catch Throwable _ nil)))

(defn- association-issues [serial-source cases]
  (let [incorporations
        (get-in serial-source [:abstract-witness :serial-history])
        by-attempt (group-by :attempt-id
                             (:observations serial-source))
        claimed (group-by :attempt-id
                          (map-indexed (fn [i entry] (assoc entry :case-index i))
                                       cases))
        expected (into {} (map (juxt :attempt-id identity) incorporations))
        unexpected (for [[id rows] claimed :when (not (contains? expected id))
                         row rows]
                     (issue :unexpected-incorporation-case (:case-index row)))
        repeated (for [[_ rows] claimed :when (> (count rows) 1)]
                   (issue :duplicate-incorporation-case
                          (:case-index (first rows))))
        missing (for [entry incorporations
                      :when (not (contains? claimed (:attempt-id entry)))]
                  (issue :missing-incorporation-case nil))
        examined
        (mapv
         (fn [index {:keys [attempt-id command-id incorporation-source]}]
           (let [expected-inc (get expected attempt-id)
                 observed (get by-attempt attempt-id)
                 binding (:binding incorporation-source)
                 record-id (try (original-command-id binding)
                                (catch Throwable _ nil))
                 component
                 (try (original-incorporation incorporation-source)
                      (catch Throwable _ nil))
                 consistent-component?
                 (and (= incorporation/report-type (:type component))
                      (= incorporation/report-version (:version component))
                      (= :bounded-incorporation-and-progression-coobservation
                         (:status component))
                      (true? (:bounded-three-way-agreement? component))
                      (= :not-issued (:certificate component))
                      (= :not-admitted (:admission component))
                      (= :none (:authority component))
                      (= :none (:transaction-authority component))
                      (= :none (:live-settlement-authority component))
                      (false? (:safe-to-retry? component))
                      (false? (:safe-to-settle? component))
                      (false? (:transaction-causality-authenticated? component))
                      (= physical-open-obligations
                         (:physical-refinement-obligations component))
                      (= :open (:physical-refinement-obligation-statuses component)))
                 receipt (private-receipt-key incorporation-source)
                 diagnostic (when (= 1 (count observed))
                              (:submission-diagnostic (first observed)))
                 key-match?
                 (and (map? receipt)
                      (= (:tx-id receipt) (:reported-tx-id diagnostic))
                      (= (:system-time receipt)
                         (:reported-system-time diagnostic))
                      (some? (:tx-id receipt))
                      (some? (:system-time receipt)))
                 problems
                 (cond-> []
                   (or (nil? expected-inc)
                       (not= command-id (:command-id expected-inc)))
                   (conj :incorporation-identity-not-in-abstract-history)
                   (not= command-id record-id)
                   (conj :command-binding-identity-diverges)
                   (not consistent-component?)
                   (conj :incorporation-progression-not-coobserved)
                   (not= 1 (count observed))
                   (conj :unmatched-or-ambiguous-submission-observation)
                   (and (= 1 (count observed)) (not key-match?))
                   (conj :reported-receipt-and-submission-key-diverge))]
             {:index index :problems problems
              :coobserved? (boolean consistent-component?)
              :key-match? (boolean key-match?)}))
         (range) cases)]
    {:issues (vec (concat unexpected repeated missing
                          (mapcat (fn [{:keys [index problems]}]
                                    (map #(issue % index) problems))
                                  examined)))
     :coobserved-count (count (filter :coobserved? examined))
     :matched-receipt-count (count (filter :key-match? examined))
     :expected-count (count incorporations)
     :supplied-count (count cases)}))

(defn compare-conditional-evidence!
  "Pure, redacted conditional composition. Supply exactly:

   {:serial-source {:abstract-witness <v1402 witness>
                    :observations [<v1403 attempt observations> ...]}
    :incorporation-cases [{:attempt-id <modeled attempt ID>
                           :command-id <modeled durable command ID>
                           :incorporation-source <v1408 private input>} ...]}

   One incarnation case is required for each modeled incorporation; other
   attempts must not be offered as positive incorporations. All join identities,
   sources and observations are caller-supplied, not authenticated. A matching
   comparison cannot prove that the same physical attempt caused a write.
   No transaction, retry, settlement, certification or read operation occurs."
  [source]
  (source-check! source)
  (let [{:keys [serial-source incorporation-cases]} source
        compared
        (try (original-serial serial-source)
             (catch Throwable _ (reject! :invalid-serial-history-evidence)))
        joined (association-issues serial-source incorporation-cases)
        reported-serial-agreement?
        (and (= serial/report-type (:type compared))
             (= serial/report-version (:version compared))
             (= (if (zero? (:expected-count joined))
                  :no-abstract-incorporations
                  :bounded-reported-metadata-order-agreement)
                (:status compared))
             (empty? (:discrepancies compared))
             (= :not-established (:physical-refinement compared))
             (false? (:attempt-to-physical-commit-authenticated? compared))
             (= :none (:authority compared))
             (= :not-issued (:certificate compared))
             (false? (:safe-to-retry? compared))
             (false? (:safe-to-settle? compared)))
        issues (cond-> (:issues joined)
                 (not reported-serial-agreement?)
                 (conj (issue :serial-metadata-correspondence-incomplete nil)))
        status (cond
                 (seq issues) :reported-evidence-divergence-or-incomplete
                 (zero? (:expected-count joined)) :no-incorporations-to-correlate
                 :else :bounded-conditional-single-node-evidence-agreement)]
    {:type report-type
     :version report-version
     :status status
     :component-statuses
     {:serial-metadata (:status compared)
      :durable-incorporation-progressions
      (if (zero? (:expected-count joined))
        :not-applicable
        (if (= (:expected-count joined) (:coobserved-count joined)
             (:supplied-count joined))
          :all-reported-cases-coobserved :incomplete-or-divergent))
      :reported-transaction-key-join
      (if (zero? (:expected-count joined))
        :not-applicable
        (if (= (:expected-count joined) (:matched-receipt-count joined)
             (:supplied-count joined))
          :all-reported-keys-agree :incomplete-or-divergent))}
     :counts {:abstract-incorporations (:expected-count joined)
              :reported-submission-observations
              (count (:observations serial-source))
              :durable-incorporation-cases (:supplied-count joined)
              :coobserved-incorporation-cases (:coobserved-count joined)
              :matching-reported-transaction-keys (:matched-receipt-count joined)
              :discrepancies (count issues)}
     :discrepancies (vec issues)
     :observation-origins
     {:abstract-events :caller-supplied-model-history
      :submission-metadata :unattested-process-local-adapter-and-ledger-copy
      :durable-command-record :caller-supplied-readback-unattested
      :read-progression :caller-supplied-basis-unattested
      :cross-report-attempt-association :caller-claimed-not-authenticated}
     :checked (:checked assurance-scope)
     :assumed (:assumed assurance-scope)
     :open-obligations (:not-established assurance-scope)
     :physical-refinement :not-established
     :physical-transaction-order-authenticated? false
     :transaction-causality-authenticated? false
     :all-writers-mediated? false
     :record-read-basis-authenticated? false
     :safe-to-retry? false
     :safe-to-settle? false
     :business-rejection-established? false
     :transaction-authority :none
     :live-settlement-authority :none
     :authority :none
     :admission :not-admitted
     :certificate :not-issued
     :proof-status :conditional-reported-evidence-diagnostic-only}))
