(ns gesso.assurance.proof.operation-writer-coverage-soundness
  "Source-rederived, nonauthoritative bridge from v1362 independent S1/S4
   induction to v1294 issued-plan, Ring-handler and host-observed entry-point
   coverage (v1366).

   Source:
     {:induction-catalog <original v1362 abstract catalog>
      :application-induction-source <original v1294 current assembly source>}

   The two families of catalogs are checked INDEPENDENTLY. A checked join
   establishes exact OPERATION-IDENTIFIER coverage only in the supplied live
   snapshot. It does not identify the actual execution semantics of a
   similarly named handler, prove a host enumerator exhaustive, establish
   physical XTDB writer/initializer completeness, or grant runtime authority.
   A host may conceal a writer; this checker has no means to discover it.

   The v1294 checker performs the source-relative identity join to issued
   executors and handlers; the v1362 checker independently rederives every
   conditional preservation premise from the original S1/S4 operation source.
   No cached input report is accepted. All output is copyable diagnostic data."
  (:require [clojure.set :as set]
            [gesso.assurance.application-induction :as application]
            [gesso.assurance.proof.abstract-execution-induction :as induction]))

(def report-version 1)
(def report-type
  :gesso.assurance.proof.operation-writer-coverage-soundness/observed-operation-join)

(def ^:private input-keys
  #{:induction-catalog :application-induction-source})
(def ^:private external-obligations
  #{:independent-v1362-and-application-v1294-operation-semantics-match
    :host-entry-point-enumerator-is-complete
    :all-direct-and-nonroute-xtdb-writers-are-enumerated
    :deployed-router-adopts-checked-snapshot
    :post-check-router-and-handler-stability
    :actual-application-initializers-are-covered
    :abstract-to-physical-xtdb-transition-refinement
    :projection-and-write-footprint-completeness
    :universal-s1-s4-checker-soundness
    :certificate-admission})

;; These functions rederive both sides from their original sources. Reject
;; substituted public Vars, including ones that manufacture plausible reports.
;; This protects an ordinary in-process caller, not arbitrary JVM corruption.
(def ^:private original-induction induction/derive-induction-schema!)
(def ^:private original-application application/check-application-induction!)

(defn- reject! [reason at details]
  (throw (ex-info "Invalid observed operation/writer coverage source."
                  (merge {:operation-writer-coverage-soundness/rejection true
                          :reason reason :at at}
                         details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- source! [source]
  (when-not (and (plain-map? source) (nil? (meta source)))
    (reject! :expected-unannotated-source-map [] {}))
  (when-not (= input-keys (set (keys source)))
    (reject! :wrong-source-keys []
             {:missing (set/difference input-keys (set (keys source)))
              :unexpected (set/difference (set (keys source)) input-keys)}))
  source)

(defn- checked-induction! [catalog]
  (when-not (identical? original-induction
                         (var-get #'induction/derive-induction-schema!))
    (reject! :substituted-induction-checker [:induction-catalog] {}))
  (let [r (original-induction catalog)]
    (when-not (and (= induction/schema-type (:type r))
                   (= induction/semantics-version (:version r))
                   (= catalog (:source-catalog r))
                   (= (:id catalog) (:catalog-id r))
                   (= (:declared-operation-ids catalog)
                      (:declared-operation-ids r))
                   (= (set (map :operation-id (:operation-cases r)))
                      (:declared-operation-ids catalog))
                   (contains? #{:conditional-declared-induction-schema
                                :open-base-or-step} (:status r))
                   (= :conditional-schema-only (:proof-status r))
                   (= :not-issued (:certificate r))
                   (= :not-admitted (:admission r))
                   (= :none (:authority r))
                   (= :not-established (:physical-operation-completeness r)))
      (reject! :induction-source-contract-changed [:induction-catalog] {}))
    r))

(defn- checked-application! [source]
  (when-not (identical? original-application
                         (var-get #'application/check-application-induction!))
    (reject! :substituted-application-checker
             [:application-induction-source] {}))
  (let [r (original-application source)]
    (when-not (and (= application/report-type (:type r))
                   (= application/report-version (:version r))
                   (= :fresh-induction-route-coverage-and-issued-plans-rechecked
                      (:checking r))
                   (= :exact-for-supplied-host-snapshot
                      (:observational-coverage r))
                   (= :exact-issued-executor-and-abstract-construction
                      (:plan-correspondence r))
                   (= (:abstract-operation-ids r)
                      (:issued-inductive-plan-ids r))
                   (= (:abstract-operation-ids r)
                      (set (keys (:checked-inductive-plans r))))
                   (= (:abstract-operation-ids r)
                      (:semantic-route-ids r))
                   (set/subset? (:semantic-route-ids r)
                                (:observed-entry-point-ids r))
                   (= (set/difference (:observed-entry-point-ids r)
                                      (:semantic-route-ids r))
                      (:observed-unclassified-entry-point-ids r))
                   (= :all-original-obligations-remain-open
                      (:original-obligation-status r))
                   (= :not-established (:physical-write-path-completeness r))
                   (= :not-established
                      (:abstract-to-physical-transition-refinement r))
                   (= :none (:transaction-authority r))
                   (= :none (:authority r)))
      (reject! :application-source-contract-changed
               [:application-induction-source] {}))
    r))

(defn derive-observed-writer-coverage!
  "Rederive both source families, compare exact operation IDs and preserve
   unclassified observed entry points. A clean snapshot is *not* a physical
   writer-closure proof; ID equality is *not* transition equivalence.

   A disagreement remains OPEN rather than being silently ignored. A
   malformed source or substituted checker fails with typed ExceptionInfo."
  [source]
  (source! source)
  (let [schema (checked-induction! (:induction-catalog source))
        application-report (checked-application!
                            (:application-induction-source source))
        abstract-ids (:declared-operation-ids schema)
        issued-ids (:abstract-operation-ids application-report)
        missing-from-issued (set/difference abstract-ids issued-ids)
        foreign-to-abstract (set/difference issued-ids abstract-ids)
        exact? (and (empty? missing-from-issued)
                    (empty? foreign-to-abstract))
        extras (:observed-unclassified-entry-point-ids application-report)
        supported? (= :conditional-declared-induction-schema (:status schema))
        rows (into []
                   (for [id (sort-by str (set/union abstract-ids issued-ids))]
                     (let [abstract (first (filter #(= id (:operation-id %))
                                                   (:operation-cases schema)))
                           issued (get (:checked-inductive-plans
                                        application-report) id)]
                       {:operation-id id
                        :in-independent-induction-catalog? (contains? abstract-ids id)
                        :in-checked-issued-application? (contains? issued-ids id)
                        :independent-step-status (:status abstract)
                        :independent-transition-fingerprint
                        (:transition-fingerprint abstract)
                        :issued-plan-transition-fingerprint
                        (:transition-fingerprint issued)
                        ;; DO NOT assert equality of fingerprints: these are
                        ;; independent source families with distinct schemas.
                        :cross-source-transition-equivalence :not-established})))
        status (cond
                 (not exact?) :open-operation-id-mismatch
                 (seq extras) :open-observed-unclassified-entry-points
                 (not supported?) :open-independent-induction-premises
                 :else :conditional-observed-operation-id-join)]
    {:type report-type
     :version report-version
     :checking :independent-v1362-induction-and-current-v1294-application-sources
     :source-induction-catalog (:induction-catalog source)
     :induction-catalog-id (:catalog-id schema)
     :application-catalog-fingerprint (:catalog-fingerprint application-report)
     :application-induction-fingerprint (:induction-fingerprint application-report)
     :application-certificate-fingerprint
     (:certificate-fingerprint application-report)
     :application-dependency-fingerprint
     (:dependency-fingerprint application-report)
     :independent-operation-ids abstract-ids
     :issued-application-operation-ids issued-ids
     :missing-issued-operation-ids missing-from-issued
     :unmodeled-issued-operation-ids foreign-to-abstract
     :exact-operation-id-match? exact?
     :operation-rows rows
     :observed-semantic-route-ids (:semantic-route-ids application-report)
     :observed-entry-point-ids (:observed-entry-point-ids application-report)
     :observed-unclassified-entry-point-ids extras
     :independent-induction-status (:status schema)
     :observed-application-status (:observational-coverage application-report)
     :status status
     :conditional-id-join? (= status :conditional-observed-operation-id-join)
     :scope :current-source-bound-host-observed-entry-point-snapshot-only
     :open-obligations (set/union external-obligations
                                  (:open-external-obligations schema)
                                  (:open-obligations application-report))
     :host-writer-enumerator-complete? :not-established
     :actual-physical-writer-completeness :not-established
     :abstract-transition-to-issued-plan-equivalence :not-established
     :initializer-coverage :not-established
     :deployed-router-adoption :not-established
     :xtdb-refinement :not-established
     :universal-checker-soundness :not-established
     :proof-status :conditional-source-join-only
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :transaction-authority :none
     :authority :none}))

(defn require-current-observed-writer-coverage!
  "Reject stale or modified diagnostic evidence by deriving a fresh source-
   relative result and comparing every field. The newly enumerated host
   snapshot is used; equality does not establish that it is exhaustive."
  [prior source]
  (let [now (derive-observed-writer-coverage! source)]
    (when-not (and (plain-map? prior)
                   (nil? (meta prior))
                   (= prior now))
      (reject! :stale-or-forged-observed-coverage-report [] {}))
    now))
