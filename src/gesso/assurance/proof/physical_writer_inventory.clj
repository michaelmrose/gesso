(ns gesso.assurance.proof.physical-writer-inventory
  "v1489: source-derived, HOST-OBSERVED inventory of declared and potential
   XTDB writers. This is an observational diagnostic, not a capability sandbox.

   Source:
     {:operation-writer-source <original v1366 source>
      :enumerate-write-paths! (fn [] {<qualified path-id>
                                    {:kind :issued-operation|:raw-xtdb|
                                           :initializer|:background-job|
                                           :external-writer|:other-writer|:unknown
                                     :operation-id <qualified op-id or nil>
                                     :writer <actual callable>}})}

   The existing v1366 checker independently rederives abstract operation,
   issued-executor and host-entry-point coverage. The additional enumerator
   MUST inspect the host's actual known writer registration surfaces; mirroring
   a hand-authored approved-writer list would not establish useful coverage.
   It is invoked freshly; writer callables themselves are NEVER invoked.

   A matching inventory can NEVER establish enumerator completeness, absence
   of hidden raw XTDB writers, host deployment, physical writer identity,
   correspondence of transaction effects, or universal refinement. The
   returned summary intentionally contains no executable writer capabilities,
   certificates, admission or transaction authority. Reports are copyable."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.operation-writer-coverage-soundness
             :as coverage]))

(def inventory-version 1)
(def inventory-type
  :gesso.assurance.proof.physical-writer-inventory/host-observed-paths)

(def ^:private source-keys
  #{:operation-writer-source :enumerate-write-paths!})
(def ^:private path-keys #{:kind :operation-id :writer})
(def ^:private allowed-kinds
  #{:issued-operation :raw-xtdb :initializer :background-job
    :external-writer :other-writer :unknown})
(def ^:private max-paths 512)
(def ^:private permanent-open-obligations
  #{:host-enumerator-exhaustiveness-and-stability
    :all-host-processes-and-external-writers-accounted-for
    :undeclared-and-raw-xtdb-bypasses-excluded
    :runtime-capability-identity-and-deployment
    :physical-execution-and-abstract-transition-correspondence
    :protected-domain-write-completeness
    :transaction-internal-prestate-and-guard-correspondence
    :universal-refinement-and-certification})

;; As in the preceding source-derived bridges, capture the original function
;; rather than letting a public Var redefine the evidence acquisition logic.
;; This is ordinary in-process hardening, NOT a defense against JVM tampering.
(def ^:private original-coverage coverage/derive-observed-writer-coverage!)

(defn- reject! [reason at]
  ;; Do not disclose source, writer functions, path IDs or application secrets.
  (throw (ex-info "Invalid Gesso physical writer inventory source."
                  {:physical-writer-inventory/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- exact-map! [x expected-keys at]
  (when-not (and (plain-map? x) (= expected-keys (set (keys x))))
    (reject! :invalid-map-shape at))
  x)

(defn- checked-source! [source]
  (when-not (identical? original-coverage
                         (var-get #'coverage/derive-observed-writer-coverage!))
    (reject! :substituted-source-checker [:operation-writer-source]))
  (let [r (original-coverage source)]
    (when-not (and (= coverage/report-type (:type r))
                   (= coverage/report-version (:version r))
                   (= (:induction-catalog source)
                      (:source-induction-catalog r))
                   (set? (:independent-operation-ids r))
                   (set? (:issued-application-operation-ids r))
                   (set? (:observed-unclassified-entry-point-ids r))
                   (= :not-established (:actual-physical-writer-completeness r))
                   (= :not-established (:xtdb-refinement r))
                   (= :not-issued (:certificate r))
                   (= :not-admitted (:admission r))
                   (= :none (:transaction-authority r))
                   (= :none (:authority r)))
      (reject! :unexpected-upstream-coverage-contract
               [:operation-writer-source]))
    r))

(defn- observed-paths! [enumerate!]
  (when-not (fn? enumerate!)
    (reject! :invalid-host-enumerator [:enumerate-write-paths!]))
  (let [paths (enumerate!)]
    (when-not (and (plain-map? paths)
                   (<= (count paths) max-paths)
                   (every? qualified-id? (keys paths)))
      (reject! :invalid-host-writer-table [:enumerate-write-paths!]))
    (doseq [[path-id entry] paths]
      (exact-map! entry path-keys [:write-paths path-id])
      (when-not (contains? allowed-kinds (:kind entry))
        (reject! :unsupported-writer-kind [:write-paths path-id :kind]))
      ;; The host supplies the actual callable to prevent a bare string/id
      ;; inventory from masquerading as a surveyed registration. We never
      ;; invoke it or return it. Its relation to a physical XTDB sink remains
      ;; a trusted premise, not authenticated evidence.
      (when-not (fn? (:writer entry))
        (reject! :noncallable-writer [:write-paths path-id :writer]))
      (if (= :issued-operation (:kind entry))
        (when-not (qualified-id? (:operation-id entry))
          (reject! :missing-issued-operation-id
                   [:write-paths path-id :operation-id]))
        (when-not (nil? (:operation-id entry))
          (reject! :nonissued-writer-claims-operation
                   [:write-paths path-id :operation-id]))))
    paths))

(defn- summarize! [original observed]
  ;; The first report is freshly derived from original application sources.
  ;; The second snapshot is host-advertised, never intrinsically exhaustive.
  (let [abstract (:independent-operation-ids original)
        issued (:issued-application-operation-ids original)
        known (set/intersection abstract issued)
        issued-paths (into {} (filter (fn [[_ v]]
                                        (= :issued-operation (:kind v)))
                                      observed))
        advertised-ids (set (map (comp :operation-id val) issued-paths))
        missing (set/difference known advertised-ids)
        foreign (set/difference advertised-ids known)
        other-paths (into {} (remove (fn [[_ v]]
                                       (= :issued-operation (:kind v)))
                                     observed))
        extra-host-entries (:observed-unclassified-entry-point-ids original)
        source-exact? (:exact-operation-id-match? original)
        declared-join? (and source-exact? (empty? missing) (empty? foreign))
        status (cond
                 (not source-exact?) :open-independent-operation-catalog-mismatch
                 (or (seq missing) (seq foreign)) :open-observed-issued-writer-mismatch
                 (seq other-paths) :open-nonissued-physical-write-paths
                 (seq extra-host-entries) :open-other-host-entry-points
                 (not (:conditional-id-join? original)) :open-upstream-coverage-premises
                 :else :conditional-observed-writer-path-id-join)
        rows (mapv (fn [[path-id {:keys [kind operation-id]}]]
                     {:path-id path-id
                      :kind kind
                      :claimed-operation-id operation-id
                      :declared-issued-operation?
                      (and (= kind :issued-operation)
                           (contains? known operation-id))
                      :physical-sink-binding :not-established})
                   (sort-by (comp str key) observed))]
    {:type inventory-type
     :version inventory-version
     :checking :fresh-independent-operation-sources-and-host-writer-survey
     :scope :only-host-enumerated-process-local-registration-surfaces
     :status status
     :induction-catalog-id (:induction-catalog-id original)
     :upstream-observed-operation-status (:status original)
     :application-catalog-fingerprint (:application-catalog-fingerprint original)
     :independent-operation-ids abstract
     :issued-operation-ids issued
     :observed-issued-operation-ids advertised-ids
     :missing-observed-issued-operation-ids missing
     :foreign-observed-issued-operation-ids foreign
     :observed-writer-path-ids (set (keys observed))
     :observed-nonissued-writer-path-ids (set (keys other-paths))
     :observed-nonissued-paths-by-kind
     (into (sorted-map)
           (for [kind (sort (disj allowed-kinds :issued-operation))]
             [kind (set (for [[id entry] other-paths
                              :when (= kind (:kind entry))] id))]))
     :observed-unclassified-entry-point-ids extra-host-entries
     :declared-to-observed-issued-ids-match? (boolean declared-join?)
     :writer-rows rows
     :open-obligations (set/union permanent-open-obligations
                                  (:open-obligations original)
                                  (when (seq missing)
                                    #{:declared-issued-writers-not-observed})
                                  (when (seq foreign)
                                    #{:observed-issued-writers-not-declared})
                                  (when (seq other-paths)
                                    #{:observed-nonissued-writers-require-accounting})
                                  (when (seq extra-host-entries)
                                    #{:unclassified-host-entry-points}))
     :host-writer-enumerator-exhaustiveness :not-established
     :writer-function-and-physical-sink-correspondence :not-established
     :complete-physical-writer-coverage :not-established
     :deployment-correspondence :not-established
     :physical-refinement :not-established
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn describe-physical-writer-inventory!
  "Recheck an original independent operation catalog and freshly enumerate
   host-advertised writer registration paths. The host's enumerator must be
   derived from the runtime registration mechanisms, not an invented second
   approved-writer list. The function DOES NOT invoke the writer callbacks.

   A clean identifier join is conditional evidence about the enumerated paths
   only. It is NEVER a closed-world proof that no other XTDB writer exists."
  [source]
  (exact-map! source source-keys [])
  (let [original (checked-source! (:operation-writer-source source))
        paths (observed-paths! (:enumerate-write-paths! source))]
    (summarize! original paths)))

(defn require-current-physical-writer-inventory!
  "Reject a copied/modified/stale diagnostic if a fresh source and path
   survey changes the reported identities or classifications. Since writer
   function identities are intentionally absent from reports, this checks
   only descriptor identity, NOT actual callable or XTDB-sink identity."
  [report source]
  (let [now (describe-physical-writer-inventory! source)]
    (when-not (and (plain-map? report) (= report now))
      (reject! :stale-or-substituted-writer-inventory []))
    now))
