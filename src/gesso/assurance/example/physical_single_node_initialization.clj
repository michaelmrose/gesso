(ns gesso.assurance.example.physical-single-node-initialization
  "v1497: Source-checked, bounded initial-state readback for the real v1494
   embedded-XTDB reservation fixture. The v1272 initial-constructor checker
   evaluates the ORIGINAL reservation catalog's Init and each invariant,
   while an independent read of the actual XTDB document must match the
   declared literal bootstrap case and the v1494 physical seed shape.

   This is a diagnostic INITIALIZATION check, not an issued initializer or
   transaction receipt. The bootstrap is an explicitly named, still unverified
   writer path. One matching external read does not establish that every
   physical initializer is enumerated, that a protected domain has complete
   writer coverage, or that XTDB transaction-internal state is authenticated.
   Never use this fixture as an authorization or certification boundary."
  (:require [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.initial-constructor :as constructor]
            [xtdb.api :as xt]))

(def initialization-type
  :gesso.assurance.example.physical-single-node-initialization/bounded-readback)
(def initialization-version 1)
(def initializer-id :slot/bootstrap)
(def ^:private declared-initializer
  {:id initializer-id
   :fields {:slot/status [:literal :available]
            :slot/active-claims [:literal 0]
            :slot/audit-tag [:literal :preserved]}})
(def ^:private required-writer-paths
  #{:initializer/bootstrap :operation/reserve :operation/release})
(def ^:private physical-fields
  #{:xt/id :revision :created-at :updated-at :status :claims :audit-tag})
(def ^:private open-obligations
  #{:actual-bootstrap-transaction-receipt-not-authenticated
    :physical-initializer-to-constructor-refinement-not-proved
    :complete-initializer-and-writer-coverage-not-established
    :physical-value-mapping-not-independently-certified
    :read-node-and-snapshot-provenance-not-authenticated
    :issued-executor-and-installed-handler-binding-not-established
    :transaction-internal-prestate-not-authenticated
    :universal-physical-refinement-not-established})

;; An ordinary public-Var replacement must not substitute success for a
;; source-derived initial-case check. This is not a checker-soundness proof.
(def ^:private original-catalog pilot/reservation-catalog)
(def ^:private original-initial-check constructor/check-initial-constructor!)
(def ^:private original-seed physical/initial-document)
(def ^:private original-projection physical/physical->abstract)
(def ^:private original-query xt/q)

(defn- reject! [reason]
  (throw (ex-info "Invalid physical single-node initialization."
                  {:physical-single-node-initialization/rejection true
                   :reason reason})))

(defn- source-integrity! []
  (when-not (and (identical? original-catalog (var-get #'pilot/reservation-catalog))
                 (identical? original-initial-check
                             (var-get #'constructor/check-initial-constructor!))
                 (identical? original-seed (var-get #'physical/initial-document))
                 (identical? original-projection
                             (var-get #'physical/physical->abstract))
                 (identical? original-query (var-get #'xt/q)))
    (reject! :substituted-initialization-source)))

(defn- original-obligation-sources! []
  (let [catalog (original-catalog)
        entries (:operations catalog)]
    (when-not (and (= :slot/reservation-lifecycle (:id catalog))
                   (= physical/operation-ids (:declared-operation-ids catalog))
                   (= physical/operation-ids (set (map :id entries)))
                   (= (count entries) (count physical/operation-ids))
                   (every? #(= (:id %) (get-in % [:construction :subject :id]))
                           entries))
      (reject! :changed-abstract-initialization-catalog))
    (let [checks (mapv (fn [{:keys [construction]}]
                         (original-initial-check
                          {:construction construction
                           :initial-condition (:initial-condition catalog)
                           :invariants (:invariants catalog)}
                          declared-initializer))
                       entries)
          expected (set (map #(vector :initial-state %)
                             (keys (:invariants catalog))))]
      (when-not (and (seq expected)
                     (every? #(and (= :source-derived-conditional-initial-case-check
                                      (:proof-status %))
                                   (= :not-issued (:certificate %))
                                   (= :none (:authority %))
                                   (= true (:initial-condition-holds? %))
                                   (= true (:all-declared-invariants-hold? %))
                                   (= expected (:checked-initial-case-ids %))
                                   (empty? (:violated-initial-case-ids %)))
                             checks)
                     (apply = (map :constructed-state checks)))
        (reject! :declared-initial-state-not-supported))
      {:catalog catalog
       :case (first checks)
       :all-obligation-fingerprints (set (map :obligation-fingerprint checks))
       :invariant-ids expected})))

(defn- checked-application! [app]
  (when-not (and (map? app)
                 (some? (:node app))
                 (string? (:slot-id app))
                 (identical? (:node app)
                             (get-in app [:biff-context :biff.xtdb/node]))
                 (= (original-catalog) (:catalog app))
                 (= physical/operation-ids (get-in app [:report :operation-ids]))
                 (= required-writer-paths
                    (get-in app [:report :declared-example-writer-paths]))
                 (= :not-established (get-in app [:report :observed-writer-completeness]))
                 (= :not-issued (get-in app [:report :certificate]))
                 (= :none (get-in app [:report :authority]))
                 (fn? (:execute! app)))
    (reject! :invalid-or-unissued-bootstrap-application))
  app)

(defn- observed-seed! [node slot-id]
  (let [rows (try
               (original-query node
                               ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ? LIMIT 2"
                                slot-id])
               (catch Exception _
                 (reject! :physical-initial-document-read-failed)))]
    (when-not (and (sequential? rows) (= 1 (count rows))
                   (map? (first rows)))
      (reject! :missing-or-ambiguous-physical-initial-document))
    (first rows)))

(defn inspect-initialization!
  "Read the v1494 slot directly from XTDB and check it against the ORIGINAL
   abstract Init, declared literal bootstrap case, and original physical seed.

   Call immediately after v1494 has seeded the node and BEFORE any reserve or
   release operation. This function performs reads only. It cannot establish
   the actual transaction that wrote the observed state or the absence of
   other writers. Returns a redacted conditional report, not proof authority."
  [app]
  (source-integrity!)
  (let [{:keys [node slot-id]} (checked-application! app)
        {:keys [case all-obligation-fingerprints invariant-ids]}
        (original-obligation-sources!)
        expected-seed (original-seed slot-id)
        expected-state (:constructed-state case)
        expected-projection (original-projection expected-seed)]
    (when-not (and (= physical-fields (set (keys expected-seed)))
                   (= slot-id (:xt/id expected-seed))
                   (zero? (:revision expected-seed))
                   (= expected-state expected-projection))
      (reject! :bootstrap-seed-outside-declared-constructor))
    (let [observed (observed-seed! node slot-id)]
      ;; Reject both extra and missing fields. On XTDB 2.2.0-beta1 the
      ;; SELECT * readback uses these normalized keyword field names.
      (when-not (and (= physical-fields (set (keys observed)))
                     (= expected-seed observed)
                     (= expected-state (original-projection observed)))
        (reject! :physical-bootstrap-readback-mismatch))
      {:type initialization-type
       :version initialization-version
       :initializer-id initializer-id
       :catalog-id :slot/reservation-lifecycle
       :case-fingerprint (:fingerprint case)
       :obligation-fingerprints all-obligation-fingerprints
       :initial-condition-holds? true
       :checked-initial-obligation-ids invariant-ids
       :observed-document-count 1
       :observed-declared-field-count (count physical-fields)
       :bootstrap-writer-listed-in-example true
       :checking :original-typed-init-and-fresh-external-physical-readback
       :scope :one-physical-bootstrap-document-at-one-read
       :proof-status :source-checked-bounded-initialization-observation
       :physical-initialization-coverage :not-established
       :complete-writer-coverage :not-established
       :internal-transaction-prestate :not-authenticated
       :open-obligations open-obligations
       :certificate :not-issued
       :admission :not-admitted
       :authority :none})))

(defn with-checked-initialized-application!
  "Manage one v1494 XTDB fixture, verify its initial readback, then call f.
   The callback and :initialization-report are valid only for that fixture's
   lifetime. No extra initialization transaction is issued."
  ([f] (with-checked-initialized-application!
         (str "v1497-" (random-uuid)) f))
  ([slot-id f]
   (when-not (fn? f)
     (reject! :invalid-initialization-callback))
   (physical/with-proving-application!
    slot-id
    (fn [app]
      (f (assoc app :initialization-report (inspect-initialization! app)))))))
