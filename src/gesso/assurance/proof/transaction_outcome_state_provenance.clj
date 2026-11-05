(ns gesso.assurance.proof.transaction-outcome-state-provenance
  "v1378: Bounded, source-rechecked outcome/state diagnostics for ONE attempted
   Gesso transaction. This joins three DIFFERENT kinds of evidence:

   (1) a v1373 process-local prepared/physical-outcome hook pair;
   (2) a v1375 comparison against separately recorded low-level XTDB ops;
   (3) a post-attempt map of reads, optionally obtained by this module's
       controlled query helper from an application-supplied XTDB node.

   Each input remains copyable and caller-originated. Even our direct XTDB
   query helper cannot authenticate its caller-supplied node, transaction
   identity, snapshot basis, absence of concurrent writers, or read timing
   relative to the claimed attempt. Reading matching persisted state DOES NOT
   establish that this attempt wrote it. A rejected attempt stays rejected;
   a commit-unknown attempt has no completed v1373 report and cannot enter
   this API. Missing/misbound observations fail closed. No certificate,
   admission, physical refinement, or transaction authority is issued.

   The input contains sensitive domain plans, bindings, and readback documents.
   Do not serialize it into browser responses or logs. Returned diagnostics
   intentionally expose only counts, mismatch indices, and source identities."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.transaction-submission-correspondence :as sub]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx-model :as tx-model]
            [xtdb.api :as xt]))

(def provenance-version 1)
(def provenance-type
  :gesso.assurance.proof.transaction-outcome-state-provenance/one-attempt)

(def ^:private input-keys
  #{:submission-source :submission-report :state-observation})
(def ^:private observed-keys
  #{:attempt-id :operation-id :command-binding :command-record-id
    :captured-physical-status :reported-receipt-present?
    :reported-receipt :observation-origin :read-phase
    :reported-read-basis :post-rows})
(def ^:private row-keys #{:target :document})
(def ^:private supported-origins
  #{:caller-supplied-unattested :gesso-xtdb-query-on-caller-node})
(def ^:private max-read-targets 64)
(def ^:private original-submission-check
  sub/require-current-recorded-submission!)
(def ^:private original-admitted tx-model/admitted-plan)
(def ^:private original-record-id resolution/record-id)

(defn- reject! [reason at]
  ;; Avoid leaking user documents, command bindings, or receipts in errors.
  (throw (ex-info "Invalid transaction outcome/state provenance evidence."
                  {:transaction-outcome-state-provenance/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x expected-keys at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= expected-keys (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- current-submission! [source report]
  (when-not (and (identical? original-submission-check
                             (var-get #'sub/require-current-recorded-submission!))
                 (identical? original-admitted (var-get #'tx-model/admitted-plan))
                 (identical? original-record-id (var-get #'resolution/record-id)))
    (reject! :substituted-upstream-checker []))
  (let [verified (original-submission-check source report)]
    (when-not (and (= sub/comparison-type (:type verified))
                   (= sub/comparison-version (:version verified))
                   (= :recorded-operations-agree (:status verified))
                   (true? (:recorded-operations-match? verified))
                   (= [] (:mismatched-op-indices verified))
                   (#{:committed :rejected} (:captured-physical-status verified))
                   (false? (:actual-low-level-submission-authenticated? verified))
                   (= :not-issued (:certificate verified))
                   (= :none (:authority verified)))
      (reject! :unusable-or-divergent-submission [:submission-report]))
    verified))

(defn- expected-rows! [source]
  ;; These are expected values from a captured plan, NOT a physical write set.
  ;; Multiple writes to one target require a more complete execution model.
  (let [acquired (:acquisition-report source)
        plan (original-admitted (:captured-prepared-plan acquired))
        commands (:commands plan)
        targets (mapv command/target commands)
        expected (mapv (fn [c] {:target (command/target c)
                                :document (command/after c)}) commands)
        record (:captured-command-record acquired)
        target [resolution/command-entity-type (:xt/id record)]]
    (when-not (and (vector? commands)
                   (<= 1 (count targets) max-read-targets)
                   (= (count targets) (count (set targets)))
                   (some #(= target %) targets)
                   (= 1 (count (filter #(= target %) targets))))
      (reject! :unsupported-or-ambiguous-write-targets [:submission-source]))
    (when-not (= record (:document (first (filter #(= target (:target %)) expected))))
      (reject! :command-record-plan-divergence [:submission-source]))
    expected))

(defn- observed-rows! [observation expected]
  (let [rows (:post-rows observation)]
    (when-not (and (vector? rows) (nil? (meta rows))
                   (= (count expected) (count rows))
                   (<= (count rows) max-read-targets))
      (reject! :missing-or-extra-post-read-targets [:state-observation :post-rows]))
    (doseq [[i [observed planned]] (map-indexed vector (map vector rows expected))]
      (exact! observed row-keys [:state-observation :post-rows i])
      (when-not (= (:target observed) (:target planned))
        (reject! :reordered-or-mismatched-read-target
                 [:state-observation :post-rows i :target]))
      (when-not (or (nil? (:document observed))
                    (and (plain-map? (:document observed))
                         (nil? (meta (:document observed)))))
        (reject! :invalid-read-document [:state-observation :post-rows i])))
    rows))

(defn- persisted-value-agrees? [expected actual]
  ;; XTDB normalizes java.time.Instant values into ZonedDateTime on a SELECT.
  ;; Compare the exact instant, not printed forms or a lossy string coercion.
  ;; All other types retain strict Clojure equality.
  (if (and (instance? java.time.Instant expected)
           (instance? java.time.ZonedDateTime actual))
    (= expected (.toInstant ^java.time.ZonedDateTime actual))
    (= expected actual)))

(defn- readback-agrees? [expected actual]
  ;; A bounded expected-field comparison, not a whole-document write frame.
  ;; XTDB may omit :xt/id as a selected column; the query target identifies it
  ;; for helper-issued reads, whereas caller-reported rows only *claim* it.
  (and (map? actual)
       (every? (fn [[key value]]
                 (if (= key :xt/id)
                   (or (not (contains? actual key))
                       (= value (get actual key)))
                   (and (contains? actual key)
                        (persisted-value-agrees? value (get actual key)))))
               expected)))

(defn- observation! [observed source verified expected]
  (exact! observed observed-keys [:state-observation])
  (let [acquired (:acquisition-report source)
        binding (:command-binding acquired)
        record-id (original-record-id binding)]
    (when-not (and (uuid? (:attempt-id observed))
                   (= (:attempt-id acquired) (:attempt-id observed))
                   (= (:attempt-id verified) (:attempt-id observed))
                   (= (:operation-id acquired) (:operation-id observed))
                   (= (:operation-id verified) (:operation-id observed))
                   (= binding (:command-binding observed))
                   (= record-id (:command-record-id observed))
                   (= record-id (:xt/id (:captured-command-record acquired)))
                   (= (:captured-physical-status acquired)
                      (:captured-physical-status observed))
                   (= (:captured-physical-status verified)
                      (:captured-physical-status observed))
                   (= (:reported-receipt-present? acquired)
                      (:reported-receipt-present? observed))
                   (= (:captured-physical-receipt acquired)
                      (:reported-receipt observed)))
      (reject! :attempt-outcome-receipt-or-binding-mismatch [:state-observation]))
    (when-not (and (contains? supported-origins (:observation-origin observed))
                   (= :after-attempt (:read-phase observed))
                   (or (nil? (:reported-read-basis observed))
                       (and (or (string? (:reported-read-basis observed))
                                (integer? (:reported-read-basis observed))
                                (uuid? (:reported-read-basis observed)))
                            (not (and (string? (:reported-read-basis observed))
                                      (str/blank? (:reported-read-basis observed)))))))
      (reject! :unsupported-read-origin-phase-or-basis [:state-observation]))
    (observed-rows! observed expected)))

(defn audit-outcome-state!
  "Join a v1375 report to bounded post-attempt reads, rechecking ALL prior
   source predicates and the complete v1373 event pair.

   Input: {:submission-source <original v1375 input>
           :submission-report <matching v1375 report>
           :state-observation
           {:attempt-id ... :operation-id ... :command-binding ...
            :command-record-id ... :captured-physical-status ...
            :reported-receipt-present? ... :reported-receipt ...
            :observation-origin :caller-supplied-unattested |
                                :gesso-xtdb-query-on-caller-node
            :read-phase :after-attempt :reported-read-basis nil|scalar
            :post-rows [{:target [table id] :document map|nil} ...]}}

   Post-rows must be in captured-plan write order, with one row per distinct
   target. Their equality only compares expected plan fields (not every
   physical field, unobserved tables, or concurrent writes). An unknown
   physical outcome cannot be presented as a completed acquisition."
  [input]
  (exact! input input-keys [])
  (let [{:keys [submission-source submission-report state-observation]} input
        verified (current-submission! submission-source submission-report)
        expected (expected-rows! submission-source)
        rows (observation! state-observation submission-source verified expected)
        mismatches (->> (map vector expected rows)
                        (keep-indexed (fn [i [e o]]
                                        (when-not (readback-agrees? (:document e)
                                                                     (:document o)) i)))
                        vec)
        absent (->> rows
                    (keep-indexed (fn [i row]
                                    (when (nil? (:document row)) i)))
                    vec)
        record-target [resolution/command-entity-type
                       (:command-record-id state-observation)]
        record-index (first (keep-indexed (fn [i row]
                                           (when (= record-target (:target row)) i))
                                         rows))
        matched? (empty? mismatches)
        outcome (:captured-physical-status verified)
        status (cond
                 (= outcome :rejected) :rejected-outcome-no-causal-attribution
                 matched? :observed-committed-projection-agrees
                 :else :observed-committed-projection-diverges)]
    {:type provenance-type
     :version provenance-version
     :status status
     :checking :source-replayed-hook-submission-and-bounded-post-readback
     :operation-id (:operation-id verified)
     :attempt-id (:attempt-id verified)
     :issued-application-fingerprint (:issued-application-fingerprint verified)
     :issued-plan-transition-fingerprint
     (:issued-plan-transition-fingerprint verified)
     :independent-transition-fingerprint
     (:independent-transition-fingerprint verified)
     :captured-physical-status outcome
     :reported-receipt-present? (:reported-receipt-present? state-observation)
     :comparison-status (:status verified)
     :post-read-origin-claim (:observation-origin state-observation)
     :post-read-origin-authenticated? false
     :post-read-phase :after-attempt
     :post-read-basis-present? (some? (:reported-read-basis state-observation))
     :post-read-basis-authenticated? false
     :transaction-internal-prestate-provenance :not-established
     :post-read-target-count (count rows)
     :post-read-matching-target-count (- (count rows) (count mismatches))
     :post-read-mismatched-target-indices mismatches
     :post-read-absent-target-indices absent
     :command-record-read-present? (some? (:document (nth rows record-index)))
     :command-record-expected-fields-match?
     (not (some #{record-index} mismatches))
     :bounded-post-state-agreement? matched?
     :post-state-caused-by-this-attempt? :not-established
     :reported-outcome-authenticated? false
     :transaction-id-authenticated? false
     :actual-submission-authenticated? false
     :complete-physical-write-footprint :not-established
     :guard-authorization :not-established
     :serial-concurrency-validation :not-established
     :committed-outcome-incorporation :not-established
     :writer-provenance-and-deployment :not-established
     :universal-execution-refinement :not-established
     :physical-refinement-obligations
     (mapv #(select-keys % [:id :status])
           (:operation-obligations (:acquisition-report submission-source)))
     :commit-unknown :not-reclassifiable-as-committed
     :proof-status :outcome-and-state-diagnostics-only
     :admission :not-admitted
     :certificate :not-issued
     :transaction-authority :none
     :authority :none}))

(defn require-current-outcome-state!
  "Rerun the complete check; a copied/altered result grants no authority."
  [input previously-reported]
  (let [current (audit-outcome-state! input)]
    (when-not (= current previously-reported)
      (reject! :stale-or-altered-outcome-state-report [:report]))
    current))

(defn capture-xtdb-poststate!
  "Issue bounded SELECT queries on a CALLER-SUPPLIED XTDB node after an attempt.

   Returns an observation for audit-outcome-state!. This helper performs real
   xt/q reads, but it does not prove the node's identity, transaction-relative
   basis, isolation, or absence of other writers; the returned maps are freely
   copyable/forgeable. A returned row is NOT an authenticated write receipt.
   Query table identifiers come only from captured-plan targets and must use
   a safe unqualified SQL identifier; arbitrary tables fail closed."
  [submission-source submission-report node]
  (let [verified (current-submission! submission-source submission-report)
        expected (expected-rows! submission-source)
        acquired (:acquisition-report submission-source)
        binding (:command-binding acquired)
        rows (mapv (fn [{:keys [target]}]
                     (let [[table id] target]
                       (when-not (and (keyword? table)
                                      (nil? (namespace table))
                                      (re-matches #"[A-Za-z_][A-Za-z0-9_]*"
                                                  (name table)))
                         (reject! :unsupported-physical-table [:submission-source]))
                       {:target target
                        :document
                        (try
                          (first (xt/q node [(str "SELECT * FROM " (name table)
                                                  " WHERE _id = ?") id]))
                          (catch Exception _
                            (reject! :physical-read-failed [:node :query])))}))
                   expected)]
    {:attempt-id (:attempt-id verified)
     :operation-id (:operation-id verified)
     :command-binding binding
     :command-record-id (original-record-id binding)
     :captured-physical-status (:captured-physical-status verified)
     :reported-receipt-present? (:reported-receipt-present? acquired)
     :reported-receipt (:captured-physical-receipt acquired)
     :observation-origin :gesso-xtdb-query-on-caller-node
     :read-phase :after-attempt
     :reported-read-basis nil
     :post-rows rows}))
