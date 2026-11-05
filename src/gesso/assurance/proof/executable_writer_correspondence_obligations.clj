(ns gesso.assurance.proof.executable-writer-correspondence-obligations
  "v1368: Source-derived EXECUTABLE WRITER CORRESPONDENCE OBLIGATION ledger.

   Recheck the original v1366 induction/application/issued-handler sources.
   For each operation, identify the independently declared abstract guard,
   input/state projections, reads, writes, frame and assignment origins, and
   enumerate the proof obligations required to relate an executable writer
   to that abstract transition. No evidence is accepted or discharged here.

   In particular: equal operation IDs, checked issued executor identities,
   copied fingerprints, or one observed physical transaction do not establish
   universal writer refinement, deployment, complete writer discovery, or
   authoritative transaction provenance. The output is copyable diagnostics.

   Conditional target (NOT proved here): every real, authorized physical
   write through an exhaustive set of current writers refines one matching
   abstract operation on its true transaction-internal state and inputs."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.operation-writer-coverage-soundness :as coverage]))

(def report-version 1)
(def report-type
  :gesso.assurance.proof.executable-writer-correspondence-obligations/ledger)

;; These are requirements, NOT evidence that any requirement holds. The
;; expected-evidence labels are descriptive identifiers for a future trusted
;; refinement checker; callers cannot satisfy them by providing report maps.
(def ^:private obligation-specs
  [{:id :typed-input-binding
    :required-evidence :executed-request-to-typed-input-projection}
   {:id :transaction-internal-prestate
    :required-evidence :authenticated-transaction-snapshot-and-typed-projection}
   {:id :guard-authorization
    :required-evidence :executed-guard-and-admitted-authorization-correspondence}
   {:id :assignment-write-footprint
    :required-evidence :executed-assignments-and-complete-physical-write-set}
   {:id :frame-and-unmodeled-fields
    :required-evidence :unchanged-state-and-physical-domain-frame-correspondence}
   {:id :serial-concurrency-validation
    :required-evidence :atomic-version-check-and-serial-commit-history}
   {:id :committed-outcome-incorporation
    :required-evidence :authenticated-commit-reject-or-unknown-resolution}
   {:id :writer-provenance-and-deployment
    :required-evidence :deployed-executor-identity-and-origin-attestation}
   {:id :universal-execution-refinement
    :required-evidence :sound-all-executions-refinement-argument}])

(def ^:private original-coverage coverage/derive-observed-writer-coverage!)
(def ^:private original-current-coverage
  coverage/require-current-observed-writer-coverage!)
(def ^:private source-keys #{:induction-catalog :application-induction-source})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid executable writer correspondence source."
                  (merge {:executable-writer-correspondence-obligations/rejection true
                          :reason reason :at at}
                         details))))

(defn- plain-map? [value]
  (or (instance? clojure.lang.PersistentArrayMap value)
      (instance? clojure.lang.PersistentHashMap value)))

(defn- source! [source]
  (when-not (and (plain-map? source)
                 (nil? (meta source))
                 (= source-keys (set (keys source))))
    (reject! :invalid-source [] {}))
  source)

(defn- checked-coverage! [source]
  (when-not (and (identical? original-coverage
                             (var-get #'coverage/derive-observed-writer-coverage!))
                 (identical? original-current-coverage
                             (var-get #'coverage/require-current-observed-writer-coverage!)))
    (reject! :substituted-coverage-checker [] {}))
  (let [r (original-coverage source)
        ids (set/union (:independent-operation-ids r)
                       (:issued-application-operation-ids r))]
    (when-not (and (= coverage/report-type (:type r))
                   (= coverage/report-version (:version r))
                   (= (:induction-catalog source) (:source-induction-catalog r))
                   (= (set (map :operation-id (:operation-rows r))) ids)
                   (= (:exact-operation-id-match? r)
                      (= (:independent-operation-ids r)
                         (:issued-application-operation-ids r)))
                   (= :not-established (:actual-physical-writer-completeness r))
                   (= :not-established (:abstract-transition-to-issued-plan-equivalence r))
                   (= :not-established (:xtdb-refinement r))
                   (= :not-admitted (:admission r))
                   (= :not-issued (:certificate r))
                   (= :none (:authority r))
                   (= :none (:transaction-authority r)))
      (reject! :unexpected-source-coverage-contract [] {}))
    r))

(defn- assignment-origin [expr state-fields input-fields]
  (case (first expr)
    :literal {:kind :literal :value (second expr)}
    :read (let [source (second expr)]
            (cond
              (contains? state-fields source)
              {:kind :prestate-read :field source}
              (contains? input-fields source)
              {:kind :operation-input-read :field source}
              :else (reject! :undeclared-abstract-assignment-origin
                             [:assignments] {:reader source})))
    (reject! :unexpected-abstract-assignment [:assignments] {})))

(defn- abstract-contract! [decl]
  ;; v1362 has independently admitted the entire declaration via v1359;
  ;; this is a source-extracted inventory, not a second implementation of
  ;; the type checker or a declaration-to-physical-plan equivalence proof.
  (let [{:keys [subject state-types input-types guard reads writes assignments]}
        decl
        state-fields (set (keys state-types))
        input-fields (set (keys input-types))]
    (when-not (and (= writes (set (keys assignments)))
                   (set/subset? writes state-fields)
                   (set/subset? reads (set/union state-fields input-fields))
                   (= :operation (:kind subject)))
      (reject! :inconsistent-admitted-declaration [:declaration] {}))
    {:subject-id (:id subject)
     :state-field-ids state-fields
     :input-field-ids input-fields
     :guard-readers (:readers guard)
     :guard-expression (:expression guard)
     :read-field-ids reads
     :write-field-ids writes
     :frame-field-ids (set/difference state-fields writes)
     :assignment-origins (into (sorted-map-by #(compare (str %1) (str %2)))
                               (map (fn [[field expr]]
                                      [field (assignment-origin expr state-fields input-fields)]))
                               assignments)}))

(defn- per-operation-obligations [abstract]
  (mapv (fn [{:keys [id required-evidence]}]
          {:id id
           :status :open
           :required-evidence required-evidence
           :source-fields
           (case id
             :typed-input-binding (:input-field-ids abstract)
             :transaction-internal-prestate (:state-field-ids abstract)
             :guard-authorization (:guard-readers abstract)
             :assignment-write-footprint (:write-field-ids abstract)
             :frame-and-unmodeled-fields (:frame-field-ids abstract)
             #{})})
        obligation-specs))

(defn derive-executable-writer-obligations!
  "Produce a source-bound, non-authoritative per-writer refinement ledger.

   Input is exactly the v1366 original source shape (not a prior report):
     {:induction-catalog ... :application-induction-source ...}

   This function rechecks current issued plans, current Ring handlers, host
   observed entry points, and the independent induction catalog by invoking
   v1366. It does NOT inspect actual transaction-internal execution or
   accept a claimed proof of any executable-writer obligation."
  [source]
  (source! source)
  (let [checked (checked-coverage! source)
        catalog (:induction-catalog source)
        by-id (into {} (map (juxt :id :declaration) (:operations catalog)))
        rows (mapv
              (fn [row]
                (let [id (:operation-id row)
                      declared? (:in-independent-induction-catalog? row)
                      issued? (:in-checked-issued-application? row)
                      declaration (get by-id id)
                      _ (when-not (= declared? (some? declaration))
                          (reject! :abstract-operation-source-mismatch
                                   [:operation-rows id] {}))
                      abstract (when declared? (abstract-contract! declaration))
                      supported? (= :conditional-composed-rule-instance
                                    (:independent-step-status row))
                      status (cond
                               (not declared?) :open-undeclared-issued-writer
                               (not issued?) :open-unissued-abstract-operation
                               (not supported?) :open-abstract-preservation
                               :else :open-executable-refinement)]
                  {:operation-id id
                   :status status
                   :declared-abstract-operation? declared?
                   :checked-issued-executor? issued?
                   :induction-step-supported? (and declared? supported?)
                   :abstract-transition-fingerprint
                   (:independent-transition-fingerprint row)
                   :issued-plan-transition-fingerprint
                   (:issued-plan-transition-fingerprint row)
                   :cross-source-transition-equivalence :not-established
                   :abstract-contract abstract
                   :obligations (per-operation-obligations abstract)
                   :executable-refinement :not-established}))
              (:operation-rows checked))
        row-ids (set (map :operation-id rows))
        expected (set/union (:independent-operation-ids checked)
                            (:issued-application-operation-ids checked))
        _ (when-not (= expected row-ids)
            (reject! :incomplete-writer-ledger [] {}))
        uncovered (:observed-unclassified-entry-point-ids checked)
        all-joined? (and (:exact-operation-id-match? checked)
                         (empty? uncovered)
                         (= :conditional-observed-operation-id-join
                            (:status checked)))
        status (cond
                 (not (:exact-operation-id-match? checked))
                 :open-operation-catalog-join
                 (seq uncovered) :open-unclassified-observed-writers
                 (not all-joined?) :open-induction-premises
                 :else :refinement-obligations-generated)]
    {:type report-type
     :version report-version
     :checking :fresh-v1366-source-rechecked-and-obligations-derived
     :source-induction-catalog catalog
     :catalog-id (:induction-catalog-id checked)
     :application-catalog-fingerprint (:application-catalog-fingerprint checked)
     :application-induction-fingerprint (:application-induction-fingerprint checked)
     :application-certificate-fingerprint (:application-certificate-fingerprint checked)
     :application-dependency-fingerprint (:application-dependency-fingerprint checked)
     :operation-rows rows
     :operation-ids row-ids
     :unmatched-abstract-ids (:missing-issued-operation-ids checked)
     :unmodeled-issued-ids (:unmodeled-issued-operation-ids checked)
     :observed-unclassified-entry-point-ids uncovered
     :status status
     :all-executable-refinement-obligations-discharged? false
     :required-obligation-ids (set (map :id obligation-specs))
     :open-obligations (set/union
                        (:open-obligations checked)
                        (set (map :id obligation-specs))
                        #{:source-to-physical-transition-refinement
                          :all-physical-writers-enumerated
                          :authenticated-transaction-observation
                          :deployed-executor-and-router-stability})
     :scope :current-source-checked-writer-ledger-only
     :physical-snapshot-authenticity :not-established
     :physical-write-path-completeness :not-established
     :handler-transition-correspondence :not-established
     :deployed-router-adoption :not-established
     :universal-checker-soundness :not-established
     :proof-status :obligations-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn require-current-executable-writer-obligations!
  "Regenerate the entire current ledger and reject stale or forged reports.
   Equality is a freshness check against supplied ORIGINAL sources, not a
   certificate of execution provenance or physical refinement."
  [prior source]
  (let [current (derive-executable-writer-obligations! source)]
    (when-not (and (plain-map? prior)
                   (nil? (meta prior))
                   (= prior current))
      (reject! :stale-or-forged-writer-obligation-report [] {}))
    current))
