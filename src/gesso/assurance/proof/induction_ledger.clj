(ns gesso.assurance.proof.induction-ledger
  "Source-derived classification of conditional induction evidence (v1284).

   A checked local inference is NOT discharge of its original obligation. This
   ledger places initial/preservation steps in a separate register from the
   COMPLETE original operation-obligation register, and partitions all open
   meta, initialization, certification and physical assumptions without
   deleting unknown future obligations. Every result is recomputed from the
   ORIGINAL v1276 abstract operation catalog, never from a supplied report.

   No theorem, certificate, dependency-closure handle, application admission,
   physical implementation correspondence, or runtime authority is issued.
   This is a COPYABLE, non-authoritative explanatory report, not a proof token.
   Formal rule soundness and real initializer/writer coverage remain open.
   The frozen checker protects against simple Var substitution, not arbitrary
   changes to the same trusted JVM or an independent proof of its soundness."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.subject :as subject]))

(def ledger-version 1)
(def ledger-type :gesso.assurance.proof.induction-ledger/conditional-evidence-ledger)
(def ^:private identity-id :gesso.assurance.proof/induction-ledger-v1)
(def ^:private checked :checked-conditional-inference)
(def ^:private open :open)
(def ^:private required-external
  #{:induction-rule-soundness
    :initial-state-existence
    :external-initializer-coverage
    :external-operation-set-completeness
    :physical-write-path-coverage
    :implementation-correspondence
    :checker-meta-soundness})

;; These are *classification labels*, not assertions that a dependency has
;; been proved. Future unknown meta obligations belong to :unclassified and
;; stay open; there is deliberately no catch-all "proved" default.
(def ^:private assumption-classes
  {:logical-soundness
   #{:induction-rule-soundness
     :checker-meta-soundness
     :independent-proof-kernel-soundness
     :proof-checker-frame-and-literal-conjunction-rule-soundness
     :proof-checker-substitution-and-frame-extensionality-soundness
     :proof-checker-and-extensionality-rule-soundness
     :proof-checker-and-literal-substitution-rule-soundness
     :initial-structural-inference-rule-soundness
     :unified-preservation-dispatch-soundness}
   :initialization
   #{:initial-state-existence
     :initial-state-coverage
     :initialization-coverage
     :external-initializer-coverage}
   :abstract-semantics
   #{:abstract-invariant-preservation}
   :physical-correspondence
   #{:external-operation-set-completeness
     :physical-write-path-coverage
     :implementation-correspondence}
   :certificate-admission
   #{:certificate-admission}})

(def ^:private original-checker induction/check-induction!)

(defn- reject! [reason details]
  (throw (ex-info "Invalid Gesso source-derived induction evidence ledger."
                  (merge {:error/type ::invalid-induction-ledger
                          :reason reason}
                         details))))

(defn- hash? [value]
  (and (string? value)
       (boolean (re-matches #"sha256:[a-f0-9]{64}" value))))

(defn- check-original! [catalog]
  ;; Never accept a candidate report instead of original source. The v1278
  ;; checker validates source shape and independently reruns its siblings.
  (let [report (original-checker catalog)
        public (var-get #'induction/check-induction!)]
    (when-not (identical? public original-checker)
      (when-not (= report (public catalog))
        (reject! :induction-checker-contract-changed {})))
    report))

(defn- report-shape! [catalog report]
  (let [ops (:declared-operation-ids report)
        invs (:declared-invariant-ids report)
        rows (:induction-outcomes report)
        evidence (:operation-evidence report)
        originals (:original-open-obligation-ids-by-operation report)
        checked-invs (:conditionally-checked-inductive-invariant-ids report)
        unsupported (:unsupported-inductive-invariant-ids report)
        status-by-invariant (into {} (map (juxt :invariant identity)) rows)]
    (when-not
      (and (= induction/report-type (:type report))
           (= induction/induction-version (:version report))
           (= (:id catalog) (:catalog-id report))
           (= (:declared-operation-ids catalog) ops)
           (= (set (keys (:invariants catalog))) invs)
           (set? ops) (seq ops) (every? keyword? ops)
           (set? invs) (seq invs) (every? keyword? invs)
           (hash? (:fingerprint report))
           (hash? (:catalog-fingerprint report))
           (vector? rows) (= (count invs) (count rows))
           (= invs (set (map :invariant rows)))
           (= (count rows) (count status-by-invariant))
           (vector? evidence) (= (count ops) (count evidence))
           (= ops (set (map :operation-id evidence)))
           (= (count evidence) (count (distinct (map :operation-id evidence))))
           (= ops (set (keys originals)))
           (every? set? (vals originals))
           (every? (fn [{:keys [operation-id original-open-obligation-ids]}]
                     (= original-open-obligation-ids (get originals operation-id)))
                   evidence)
           (= invs (set/union checked-invs unsupported))
           (empty? (set/intersection checked-invs unsupported))
           (= (empty? unsupported)
              (:all-declared-invariants-conditionally-inductive? report))
           (set? (:open-meta-obligations report))
           (set/subset? required-external (:open-meta-obligations report))
           (= :exact-base-and-all-declared-transitions (:proof-rule report))
           (= :closed-declared-abstract-machine-only (:scope report))
           (= :restricted-conditional-induction-candidates-checked
              (:proof-status report))
           (= :not-independently-proved (:checker-soundness report))
           (= :not-issued (:certificate report))
           (= :not-admitted (:admission report))
           (= :not-established (:runtime-enforcement report))
           (= :not-established (:implementation-correspondence report))
           (= :none (:authority report)))
      (reject! :induction-report-contract-changed {}))
    (doseq [{:keys [invariant initial-obligation-id
                    preservation-obligation-id initial-status
                    preserving-operation-ids unsupported-operation-ids
                    status reason] :as row} rows]
      (when-not (and (= [:initial-state invariant] initial-obligation-id)
                     (= [:preservation invariant] preservation-obligation-id)
                     (contains? #{checked open} initial-status)
                     (set? preserving-operation-ids)
                     (set? unsupported-operation-ids)
                     (= ops (set/union preserving-operation-ids
                                       unsupported-operation-ids))
                     (empty? (set/intersection preserving-operation-ids
                                               unsupported-operation-ids))
                     (= (and (= checked initial-status)
                             (empty? unsupported-operation-ids))
                        (= :checked-conditional-induction status))
                     (= (= status :checked-conditional-induction)
                        (contains? checked-invs invariant))
                     (contains? #{:checked-conditional-induction open} status)
                     (= (cond
                          (= open initial-status) :initial-consequence-open
                          (seq unsupported-operation-ids) :some-preservation-open
                          :else nil)
                        reason)
                     (= #{:initial-obligation-id :initial-rule :initial-status
                          :preservation-obligation-id :preserving-operation-ids
                          :unsupported-operation-ids :status :invariant :reason}
                        (set (keys row))))
        (reject! :induction-step-contract-changed {:invariant invariant})))
    report))

(defn- classify-open-meta! [obligations]
  (let [classified (into {} (map (fn [[kind ids]]
                                  [kind (set/intersection obligations ids)])
                                assumption-classes))
        seen (reduce set/union #{} (vals classified))
        unknown (set/difference obligations seen)
        categories (assoc classified :unclassified unknown)]
    ;; Classification may be augmented, never used to silently *remove* an
    ;; inherited open assumption. Keep every original keyword exactly once.
    (when-not (and (= obligations (reduce set/union #{} (vals categories)))
                   (= (reduce + (map count (vals categories)))
                      (count obligations)))
      (reject! :assumption-classification-incomplete {}))
    categories))

(defn- initial-step [row]
  {:obligation-id (:initial-obligation-id row)
   :status (:initial-status row)
   :rule (when (= checked (:initial-status row)) (:initial-rule row))})

(defn- preservation-steps [row operations]
  (into (sorted-map-by (fn [a b] (compare (str a) (str b))))
        (map (fn [id]
               [id {:obligation-id (:preservation-obligation-id row)
                    :status (if (contains? (:preserving-operation-ids row) id)
                              checked open)}]))
        (sort-by str operations)))

(defn- invariant-step [operations row]
  {:invariant-id (:invariant row)
   :initial (initial-step row)
   :preservation-by-operation (preservation-steps row operations)
   :induction-status (:status row)
   :open-reason (:reason row)})

(defn- original-register! [report]
  (let [register (:original-open-obligation-ids-by-operation report)
        expected-ops (:declared-operation-ids report)
        expected-initial (set (map #(vector :initial-state %)
                                   (:declared-invariant-ids report)))
        expected-pres (set (map #(vector :preservation %)
                                (:declared-invariant-ids report)))]
    (when-not
      (and (= expected-ops (set (keys register)))
           (every? (fn [[_ ids]]
                     (and (set? ids)
                          (set/subset? expected-initial ids)
                          (set/subset? expected-pres ids)
                          (every? (fn [id]
                                    (and (vector? id) (= 2 (count id))
                                         (contains? #{:initial-state :preservation
                                                      :assumption :implementation}
                                                    (first id))))
                                  ids)))
                   register))
      (reject! :original-obligation-ledger-changed {}))
    register))

(defn check-induction-ledger!
  "Build a COPYABLE, unadmitted evidence classification from ORIGINAL catalog.

   :checked-abstract-steps records only restricted conditional inferences.
   :open-abstract-steps records the unsupported base/transition cases.
   :original-open-obligation-ids-by-operation retains ALL original obligations
   even when a corresponding conditional inference was checked.
   :open-assumptions partitions the COMPLETE meta ledger, including unknown
   new kinds in :unclassified. No assumption is discharged by classification.

   No supplied candidate report, graph, certificate or trust flag is accepted."
  [catalog]
  (let [report (report-shape! catalog (check-original! catalog))
        operations (:declared-operation-ids report)
        rows (mapv #(invariant-step operations %)
                   (:induction-outcomes report))
        checked-steps (mapv #(assoc % :step-class :checked-conditional-abstract)
                            (filter (fn [r]
                                      (= :checked-conditional-induction
                                         (:induction-status r))) rows))
        open-steps (mapv #(assoc % :step-class :open-abstract-inference)
                         (filter (fn [r]
                                   (= open (:induction-status r))) rows))
        original-register (original-register! report)
        open-meta (:open-meta-obligations report)
        open-classes (classify-open-meta! open-meta)
        identity (subject/describe-subject
                  {:kind :rule :id identity-id
                   :interface {:ledger-version ledger-version
                               :induction-fingerprint (:fingerprint report)
                               :catalog-fingerprint (:catalog-fingerprint report)
                               :checked-steps checked-steps
                               :open-steps open-steps
                               :original-open-obligations original-register
                               :open-meta-obligations open-meta
                               :open-assumptions open-classes}})]
    {:type ledger-type
     :version ledger-version
     :fingerprint (:fingerprint identity)
     :catalog-id (:catalog-id report)
     :catalog-fingerprint (:catalog-fingerprint report)
     :induction-fingerprint (:fingerprint report)
     :declared-operation-ids operations
     :declared-invariant-ids (:declared-invariant-ids report)
     :checked-abstract-steps checked-steps
     :open-abstract-steps open-steps
     :all-declared-invariants-conditionally-inductive?
     (:all-declared-invariants-conditionally-inductive? report)
     :original-open-obligation-ids-by-operation original-register
     :original-obligation-status :all-original-obligations-remain-open
     :open-meta-obligations open-meta
     :open-assumptions open-classes
     :assumption-discharge :not-performed
     :source-dependency-closure :not-established-by-this-ledger
     :checking :original-source-induction-rechecked
     :scope :closed-declared-abstract-machine-only
     :proof-status (if (empty? open-steps)
                     :checked-conditional-abstract-induction-evidence
                     :partial-conditional-induction-evidence)
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))

(defn require-current-induction-ledger!
  "Recompute all evidence and classifications from CURRENT ORIGINAL source.
   Reject copied/modified ledger maps or stale source. Returns fresh copyable
   explanation; never returns a certificate or reactivates an old proof."
  [prior current-catalog]
  (let [now (check-induction-ledger! current-catalog)]
    (when-not (and (map? prior) (not (record? prior))
                   (nil? (meta prior)) (= prior now))
      (reject! :stale-or-forged-induction-ledger
               {:expected (:fingerprint now)
                :actual (:fingerprint prior)}))
    now))
