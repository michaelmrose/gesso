(ns gesso.live.application-preflight.authoritative-coverage
  "v1292: observe authoritative application entry-point coverage without
   confusing a complete DECLARED operation catalog with a complete PHYSICAL
   write-path inventory.

   This module accepts a current v1288 inductive application contract *source*
   and a host-provided entry-point enumerator. On each check it invokes that
   enumerator and requires every declared semantic route to appear with the
   SAME issued Ring handler object already checked by v1253/v1288. Every
   additional observed entry point remains unclassified and uncovered; a
   caller cannot assert that it is read-only or certified through this API.

   Source/handler equality is established only for the enumerated snapshot.
   This cannot prove the host enumerator is exhaustive, discover a hidden
   direct XTDB writer, certify the deployed router, or prove transaction
   refinement. Even an observed set containing *no* extra entries leaves
   physical writer completeness NOT ESTABLISHED. Reports are explanatory
   data, never certificates, runtime permissions, or durable audit receipts.

   Source shape:
   {:inductive-contract-source <exact v1288 source>
    :enumerate-entry-points!  (fn [] {qualified-route-id
                                      {:kind :semantic-route|:unclassified
                                       :handler actual-ring-callable}})}

   The enumerator must be supplied by the host that owns the actual router;
   mirroring a manually maintained second registry is NOT a correctness
   argument. Its identity, behavior, and completeness are trusted."
  (:require [clojure.set :as set]
            [gesso.live.application-preflight.inductive-contract :as inductive]))

(def coverage-version 1)
(def report-type
  :gesso.live.application-preflight.authoritative-coverage/observed-report)

(def ^:private input-keys
  #{:inductive-contract-source :enumerate-entry-points!})
(def ^:private entry-keys #{:kind :handler})
(def ^:private entry-kinds #{:semantic-route :unclassified})
(def ^:private permanent-open-obligations
  #{:host-entry-point-enumerator-is-complete-and-current
    :non-route-and-direct-executor-writers-are-enumerated
    :all-xtdb-write-paths-refine-declared-abstract-operations
    :deployed-router-adopts-checked-snapshot
    :post-check-router-and-entry-point-stability
    :actual-runtime-authorization-and-transaction-atomicity})

(defn- fail! [reason at details]
  (throw (ex-info "Invalid Gesso observable authoritative entry-point coverage."
                  (merge {:error/type ::invalid-authoritative-coverage
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [value expected at]
  (when-not (and (plain-map? value) (nil? (meta value)))
    (fail! :expected-unannotated-plain-map at {}))
  (let [actual (set (keys value))]
    (when-not (= actual expected)
      (fail! :wrong-keys at
             {:missing (set/difference expected actual)
              :unexpected (set/difference actual expected)})))
  value)

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

;; A public sibling Var can be redefined in a test/host JVM. Never use an
;; unverified success map in place of the original source-deriving checker.
;; This is not a defense against malicious bytecode or private-Var mutation.
(def ^:private original-inductive-checker
  inductive/check-inductive-contract!)

(defn- current-contract! [source]
  (let [public (var-get #'inductive/check-inductive-contract!)
        actual (original-inductive-checker source)]
    (when-not (identical? original-inductive-checker public)
      (when-not (= actual (public source))
        (fail! :substituted-inductive-checker-report
               [:inductive-contract-source] {})))
    (when-not (and (= inductive/report-type (:type actual))
                   (= inductive/contract-version (:version actual))
                   (= :current-abstract-source-and-issued-route-snapshot-rechecked
                      (:checking actual))
                   (= :all-original-obligations-remain-open
                      (:original-obligation-status actual))
                   (= :not-established (:physical-writer-completeness actual))
                   (= :none (:transaction-authority actual))
                   (= :none (:authority actual)))
      (fail! :invalid-inductive-contract [:inductive-contract-source] {}))
    actual))

(defn- observed-entries! [enumerator]
  (when-not (fn? enumerator)
    (fail! :invalid-entry-point-enumerator [:enumerate-entry-points!] {}))
  (let [observed (enumerator)]
    (when-not (and (plain-map? observed)
                   (nil? (meta observed))
                   (every? qualified-id? (keys observed)))
      (fail! :invalid-observed-entry-point-table
             [:enumerate-entry-points!] {}))
    (doseq [[id entry] observed]
      (exact-map! entry entry-keys [:entry-points id])
      (when-not (contains? entry-kinds (:kind entry))
        (fail! :unsupported-entry-point-kind
               [:entry-points id :kind] {:kind (:kind entry)}))
      (when-not (fn? (:handler entry))
        (fail! :noncallable-entry-point
               [:entry-points id :handler] {})))
    observed))

(defn check-authoritative-coverage!
  "Rederive coverage from CURRENT original proof/application sources and one
   fresh host-enumerated entry-point snapshot.

   Reject a missing, relabeled, or handler-swapped declared semantic route.
   Enumerated extras are reported as UNCLASSIFIED, not silently discarded or
   treated as evidence of a complete physical writer inventory.

   The returned map has NO transaction authority; its :observed-* fields
   describe only this call's host-provided snapshot. No report is an admissible
   substitute for fresh enumeration at the time of use."
  [source]
  (exact-map! source input-keys [])
  (let [{:keys [inductive-contract-source enumerate-entry-points!]} source
        checked (current-contract! inductive-contract-source)
        ;; v1288 itself verifies exact semantic operation/handler/route binding.
        route-handlers (:semantic-route-handlers inductive-contract-source)
        declared (:semantic-route-ids checked)
        observed (observed-entries! enumerate-entry-points!)
        observed-ids (set (keys observed))
        missing (set/difference declared observed-ids)
        extras (set/difference observed-ids declared)]
    (when (seq missing)
      (fail! :missing-observed-semantic-routes
             [:enumerate-entry-points!]
             {:missing-route-ids missing}))
    (doseq [id declared]
      (let [{:keys [kind handler]} (get observed id)]
        (when-not (= :semantic-route kind)
          (fail! :misclassified-semantic-route [:entry-points id]
                 {:actual-kind kind}))
        (when-not (identical? handler (get route-handlers id))
          (fail! :substituted-semantic-route-handler [:entry-points id]
                 {}))))
    (doseq [id extras]
      (when-not (= :unclassified (:kind (get observed id)))
        (fail! :undeclared-semantic-route [:entry-points id]
               {})))
    (let [open (set/union permanent-open-obligations
                          (:open-obligations checked)
                          (when (seq extras)
                            #{:observed-entry-points-outside-inductive-contract}))]
      {:type report-type
       :version coverage-version
       :checking :fresh-source-derived-contract-and-host-enumeration
       :catalog-fingerprint (:catalog-fingerprint checked)
       :induction-fingerprint (:induction-fingerprint checked)
       :certificate-fingerprint (:certificate-fingerprint checked)
       :dependency-fingerprint (:dependency-fingerprint checked)
       :abstract-operation-ids (:abstract-operation-ids checked)
       :declared-semantic-route-ids declared
       :observed-entry-point-ids observed-ids
       :observed-matched-semantic-route-ids declared
       :observed-unclassified-entry-point-ids extras
       :observable-semantic-route-coverage :exact-for-supplied-snapshot
       :unclassified-entry-point-status (if (seq extras)
                                          :observed-uncovered
                                          :none-observed)
       :physical-entry-point-exhaustiveness :not-established
       :physical-write-path-completeness :not-established
       :abstract-to-physical-transition-correspondence :not-established
       :active-router-adoption :not-established
       :source-relative-application-binding :rechecked-current
       :scope :only-host-enumerated-entry-points
       :original-open-obligation-ids-by-operation
       (:original-open-obligation-ids-by-operation checked)
       :original-obligation-status :all-original-obligations-remain-open
       :open-premises (:open-premises checked)
       :open-obligations open
       :production-certification :not-established
       :transaction-authority :none
       :authority :none})))
