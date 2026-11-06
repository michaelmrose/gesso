(ns gesso.assurance.proof.application-writer-coverage
  "v1446: connect a live v1440 DECLARED application closure with the existing
   v1366 independent-operation / issued-executor / host-observed entry-point
   check. Do not invent a second writer registry or accept cached success maps.

   The join compares operation identifiers in three independently rederived
   source families. It reports omissions, unmodeled issued operations, and
   unclassified observed entries without inferring physical completeness.

   Even exact identity coverage does NOT establish semantic equivalence of
   the original operation declarations, that the host enumerator is exhaustive,
   or that any real XTDB writer refines the abstract transition. Outputs are
   copyable diagnostics, not certificates, admission handles, or authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.application-dependency :as application]
            [gesso.assurance.proof.operation-writer-coverage-soundness :as writer]))

(def coverage-version 1)
(def coverage-type
  :gesso.assurance.proof.application-writer-coverage/source-relative-observed-join)

(def ^:private source-keys
  #{:induction-catalog :application-induction-source})

(def ^:private permanent-open-obligations
  #{:v1440-v1362-operation-source-semantic-correspondence
    :host-observed-entry-point-enumerator-exhaustiveness
    :all-direct-and-nonroute-xtdb-writers-enumerated
    :actual-physical-writer-completeness
    :abstract-to-issued-plan-transition-equivalence
    :actual-transaction-internal-state-and-input-correspondence
    :all-physical-transitions-refine-the-abstract-machine
    :deployed-router-and-handler-adoption
    :post-check-application-and-writer-stability
    :universal-proof-kernel-soundness
    :certificate-admission})

;; Protect the source-derived checker from routine in-process public Var
;; substitution. This is not a JVM sandbox against reflection/bytecode changes.
(def ^:private original-writer-check writer/derive-observed-writer-coverage!)

(defn- reject! [reason context]
  (throw (ex-info "Invalid Gesso declared application/writer coverage."
                  (merge {:error/type ::invalid-application-writer-coverage
                          :reason reason}
                         context))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- input! [source]
  (when-not (and (plain-map? source)
                 (nil? (meta source))
                 (= source-keys (set (keys source))))
    (reject! :invalid-original-writer-source
             {:expected source-keys}))
  source)

(defn- checked-writer! [source]
  (when-not (identical? original-writer-check
                         (var-get #'writer/derive-observed-writer-coverage!))
    (reject! :substituted-writer-coverage-checker {}))
  (let [report (original-writer-check source)
        independent (:independent-operation-ids report)
        issued (:issued-application-operation-ids report)
        observed (:observed-entry-point-ids report)
        semantic (:observed-semantic-route-ids report)
        extras (:observed-unclassified-entry-point-ids report)]
    (when-not (and (= writer/report-type (:type report))
                   (= writer/report-version (:version report))
                   (= (:induction-catalog source)
                      (:source-induction-catalog report))
                   (set? independent) (seq independent)
                   (set? issued) (seq issued)
                   (set? observed) (set? semantic) (set? extras)
                   (set/subset? semantic observed)
                   (= extras (set/difference observed semantic))
                   (= (:exact-operation-id-match? report)
                      (= independent issued))
                   (= independent
                      (set (map :operation-id
                                (filter :in-independent-induction-catalog?
                                        (:operation-rows report)))))
                   (= issued
                      (set (map :operation-id
                                (filter :in-checked-issued-application?
                                        (:operation-rows report)))))
                   (= :not-established (:actual-physical-writer-completeness report))
                   (= :not-established
                      (:abstract-transition-to-issued-plan-equivalence report))
                   (= :not-established (:xtdb-refinement report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report))
                   (= :none (:transaction-authority report))
                   (= :none (:authority report)))
      (reject! :unexpected-writer-coverage-contract {}))
    report))

(defn- differences [left right]
  {:missing-from-right (set/difference left right)
   :unmodeled-in-right (set/difference right left)
   :exact? (= left right)})

(defn- row [id declared independent issued]
  {:operation-id id
   :in-declared-application? (contains? declared id)
   :in-independent-induction? (contains? independent id)
   :in-issued-application? (contains? issued id)
   :abstract-source-equivalence :not-established
   :installed-handler-semantic-equivalence :not-established
   :physical-transition-refinement :not-established})

(defn- assemble-snapshot-report!
  "Pure, NONAUTHORITATIVE three-catalog set accounting. Input reports are
   accepted only here as data; the public source path never takes such reports."
  [declared writer-report]
  (let [        ids (:operation-ids declared)
        independent (:independent-operation-ids writer-report)
        issued (:issued-application-operation-ids writer-report)
        abstract-join (differences ids independent)
        issued-join (differences ids issued)
        independent-join (differences independent issued)
        extras (:observed-unclassified-entry-point-ids writer-report)
        all-ids (set/union ids independent issued)
        exact? (and (:exact? abstract-join)
                    (:exact? issued-join)
                    (:exact? independent-join))
        conditional? (and exact?
                          (empty? extras)
                          (:conditional-id-join? writer-report))
        open-obligations
        (set/union permanent-open-obligations
                   (:open-meta-obligations declared)
                   (:open-obligations writer-report)
                   (when-not exact?
                     #{:declared-independent-and-issued-operation-id-mismatch})
                   (when (seq extras)
                     #{:observed-entry-points-outside-declared-application}))]
    (when-not (and (set? ids) (seq ids) (set? all-ids)
                   (= (:operation-ids declared)
                      (set (keys (:original-open-obligation-ids-by-operation
                                  declared))))
                   (= :not-established (:installed-writer-coverage declared))
                   (= :not-issued (:certificate declared))
                   (= :not-admitted (:admission declared))
                   (= :none (:authority declared)))
      (reject! :unexpected-application-closure-contract {}))
    {:type coverage-type
     :version coverage-version
     :checking :current-declared-source-and-independent-issued-host-snapshot
     :scope :supplied-live-declared-application-and-host-observation
     :catalog-id (:catalog-id declared)
     :catalog-fingerprint (:catalog-fingerprint declared)
     :induction-fingerprint (:induction-fingerprint declared)
     :application-dependency-ledger-fingerprint (:ledger-fingerprint declared)
     :writer-source-catalog-id (:induction-catalog-id writer-report)
     :writer-application-catalog-fingerprint
     (:application-catalog-fingerprint writer-report)
     :declared-operation-ids ids
     :independent-operation-ids independent
     :issued-operation-ids issued
     :declared-vs-independent abstract-join
     :declared-vs-issued issued-join
     :independent-vs-issued independent-join
     :exact-observed-operation-id-join? exact?
     :operation-rows (mapv #(row % ids independent issued)
                           (sort-by str all-ids))
     :observed-semantic-route-ids
     (:observed-semantic-route-ids writer-report)
     :observed-entry-point-ids (:observed-entry-point-ids writer-report)
     :observed-unclassified-entry-point-ids extras
     :host-snapshot-status (if (seq extras)
                             :contains-unclassified-observed-entries
                             :no-unclassified-entries-observed)
     :status (cond
               (not exact?) :open-operation-catalog-mismatch
               (seq extras) :open-unclassified-observed-entry-points
               conditional? :conditional-observed-operation-id-join
               :else :open-independent-source-premises)
     :original-open-obligation-ids-by-operation
     (:original-open-obligation-ids-by-operation declared)
     :open-assumptions (:open-assumptions declared)
     :open-obligations open-obligations
     :assumption-discharge :not-performed
     :declaration-semantic-equivalence :not-established
     :host-enumerator-exhaustiveness :not-established
     :physical-writer-coverage :not-established
     :abstract-to-physical-correspondence :not-established
     :deployment-correspondence :not-established
     :universal-kernel-soundness :not-established
     :proof-status :conditional-snapshot-id-join-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn describe-application-writer-coverage!
  "Revalidate the current original catalog and live v1440 closure, then
   independently check the original v1366 induction/application/host sources.
   The host is freshly enumerated through its own trusted-source boundary.
   Results remain observations, not certificates or installed-writer proofs."
  [catalog current-graph closure writer-source]
  (input! writer-source)
  (application/require-current-application-dependencies!
   closure catalog current-graph)
  (let [declared (application/describe-application-dependencies closure)
        writer-report (checked-writer! writer-source)]
    (assemble-snapshot-report! declared writer-report)))

(defn require-current-application-writer-coverage!
  "Recompute from original sources and a FRESH host snapshot. A report copy
   has no authority; this function only checks whether it remains descriptive
   of that newly checked snapshot. Any modification or stale source rejects."
  [report catalog current-graph closure writer-source]
  (let [now (describe-application-writer-coverage!
             catalog current-graph closure writer-source)]
    (when-not (and (plain-map? report)
                   (nil? (meta report))
                   (= report now))
      (reject! :stale-or-substituted-coverage-report {}))
    now))
