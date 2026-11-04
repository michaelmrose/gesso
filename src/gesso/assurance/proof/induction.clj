(ns gesso.assurance.proof.induction
  "Source-derived, unadmitted induction-candidate checker (v1278).

   (check-induction! <original v1276 operation-set catalog>)

   Check an exact, nonempty CLOSED DECLARED set of abstract operations. For
   EACH invariant require BOTH an original-source v1270 initial-consequence
   inference and an original-source v1269 unified-preservation inference for
   EVERY operation. Never treat a partial conjunction of proof-family results
   as a whole-invariant derivation. An unsupported base or even one unsupported
   operation keeps the invariant open. A source change invalidates identity.

   A completed candidate is a CONDITIONAL abstract induction argument, not an
   admitted proof certificate. Logical rule soundness, initial-state existence
   and coverage, completeness of the *physical* operation set, XTDB/CLJS
   implementation correspondence, and authority all remain unestablished.
   The original obligation ledgers are not mutated or discharged. SHA-256
   report fingerprints are copyable semantic identities, never credentials.

   Public sibling-checker substitutions are checked against implementations
   captured when this namespace loads. This detects with-redefs-style report
   tampering, not bugs in those implementations or malicious classpath edits."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.initial :as initial]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.unified-preservation :as preservation]
            [gesso.assurance.subject :as subject]))

(def induction-version 1)
(def report-type :gesso.assurance.proof.induction/unadmitted-induction-candidate)

(def ^:private identity-id :gesso.assurance.proof/induction-candidate-v1)
(def ^:private checked :checked-conditional-inference)
(def ^:private open :open)
(def ^:private expected-categories
  #{:initial-state :preservation :assumption :implementation})
(def ^:private expected-catalog-meta
  #{:external-operation-set-completeness
    :physical-write-path-coverage
    :initialization-coverage
    :abstract-invariant-preservation
    :checker-meta-soundness
    :certificate-admission})

(defn- reject! [reason details]
  (throw (ex-info "Invalid Gesso source-derived abstract induction candidate."
                  (merge {:error/type ::invalid-induction
                          :reason reason} details))))

(defn- sha256? [x]
  (and (string? x) (boolean (re-matches #"sha256:[0-9a-f]{64}" x))))

;; Frozen source-derived implementations, NOT an independently trusted kernel.
;; The public result is never used to establish the candidate unless it equals
;; an independently rerun frozen implementation when that public Var changes.
(def ^:private original-catalog-checker operation-set/check-operation-set!)
(def ^:private original-initial-checker initial/check-initial-consequence!)
(def ^:private original-preservation-checker preservation/check-unified-preservation!)
(def ^:private original-obligation-checker obligation/derive-obligations!)

(defn- checked-sibling! [label public-var original source]
  (let [public (var-get public-var)]
    (if (identical? public original)
      (original source)
      (let [reported (public source)
            expected (original source)]
        (when-not (= reported expected)
          (reject! :sibling-checker-contract-changed {:checker label}))
        expected))))

(defn- source-ledger! [source]
  (checked-sibling! :obligation #'obligation/derive-obligations!
                    original-obligation-checker source))

(defn- catalog-report! [catalog]
  (checked-sibling! :operation-set #'operation-set/check-operation-set!
                    original-catalog-checker catalog))

(defn- initial-report! [source]
  (checked-sibling! :initial #'initial/check-initial-consequence!
                    original-initial-checker source))

(defn- preservation-report! [source]
  (checked-sibling! :unified-preservation
                    #'preservation/check-unified-preservation!
                    original-preservation-checker source))

(defn- validate-catalog! [catalog report]
  (let [results (:operation-results report)
        ids (:declared-operation-ids catalog)
        invariant-ids (set (keys (:invariants catalog)))]
    (when-not
      (and (= operation-set/report-type (:type report))
           (= operation-set/operation-set-version (:version report))
           (sha256? (:fingerprint report))
           (= (:id catalog) (:catalog-id report))
           (= ids (:declared-operation-ids report))
           (= :checked (:exact-declared-operation-coverage report))
           (= invariant-ids (:invariant-ids report))
           (vector? results)
           (= (count ids) (count results))
           (= ids (set (map :operation-id results)))
           (= (count results) (count (distinct (map :operation-id results))))
           (= ids (set (keys (:open-obligation-ids-by-operation report))))
           (every? (fn [{:keys [operation-id open-obligation-ids]}]
                     (= open-obligation-ids
                        (get (:open-obligation-ids-by-operation report)
                             operation-id)))
                   results)
           (= :declared-abstract-operation-set-checked (:proof-status report))
           (= :closed-declared-abstract-operation-set-only (:scope report))
           (= expected-catalog-meta (:open-meta-obligations report))
           (= :not-established (:external-operation-set-completeness report))
           (= :not-established (:physical-write-path-coverage report))
           (= :not-established (:initialization-coverage report))
           (= :not-established (:invariant-preservation report))
           (= :not-independently-proved (:checker-soundness report))
           (= :not-issued (:certificate report))
           (= :not-admitted (:admission report))
           (= :not-established (:runtime-enforcement report))
           (= :not-established (:implementation-correspondence report))
           (= :none (:authority report)))
      (reject! :operation-set-contract-changed {}))
    (into {} (map (juxt :operation-id identity)) results)))

(defn- validate-ledger! [source catalog-entry ledger invariant-ids]
  (let [all-obligations (mapcat identity (vals (:obligations ledger)))
        expected-init (set (map #(vector :initial-state %) invariant-ids))
        expected-pres (set (map #(vector :preservation %) invariant-ids))]
    (when-not
      (and (= obligation/obligation-type (:type ledger))
           (= obligation/obligation-version (:version ledger))
           (= source (:source ledger))
           (sha256? (:fingerprint ledger))
           (= (:fingerprint ledger) (:obligation-fingerprint catalog-entry))
           (= (:transition-fingerprint ledger)
              (:transition-fingerprint catalog-entry))
           (= (:subject-fingerprint ledger)
              (:subject-fingerprint catalog-entry))
           (= (:open-obligation-ids ledger)
              (:open-obligation-ids catalog-entry))
           (= invariant-ids (set (keys (:invariants ledger))))
           (= expected-categories (set (keys (:obligations ledger))))
           (= expected-init
              (set (map :id (get-in ledger [:obligations :initial-state]))))
           (= expected-pres
              (set (map :id (get-in ledger [:obligations :preservation]))))
           (= (:open-obligation-ids ledger)
              (set (map :id all-obligations)))
           (= (count all-obligations)
              (count (:open-obligation-ids ledger)))
           (every? #(= :open (:status %)) all-obligations)
           (= :all-obligations-open (:status ledger))
           (= :not-proved (:proof-status ledger))
           (= :not-issued (:certificate ledger))
           (= :not-admitted (:admission ledger))
           (= :none (:authority ledger)))
      (reject! :obligation-ledger-contract-changed
               {:operation-id (get-in source [:construction :subject :id])}))
    ledger))

(defn- validate-outcomes! [kind outcomes invariant-ids expected-prefix]
  (let [id-key (case kind :initial :obligation-id :preservation :obligation-id)
        ids (mapv id-key outcomes)
        expected (set (map #(vector expected-prefix %) invariant-ids))]
    (when-not (and (vector? outcomes)
                   (= (count expected) (count outcomes))
                   (= expected (set ids))
                   (= (count outcomes) (count (distinct ids)))
                   (every? (fn [{:keys [invariant status] :as outcome}]
                             (and (= (get outcome id-key)
                                     [expected-prefix invariant])
                                  (contains? #{checked open} status)))
                           outcomes))
      (reject! :inference-outcome-coverage-lost {:kind kind}))
    (into {} (map (juxt :invariant identity) outcomes))))

(defn- validate-initial! [r ledger invariant-ids]
  (let [by-id (validate-outcomes! :initial (:initial-outcomes r)
                                  invariant-ids :initial-state)
        success (set (map :obligation-id
                          (filter #(= checked (:status %))
                                  (:initial-outcomes r))))
        open-ids (set (map :obligation-id
                           (filter #(= open (:status %))
                                   (:initial-outcomes r))))]
    (when-not
      (and (= initial/report-type (:type r))
           (= initial/initial-version (:version r))
           (sha256? (:fingerprint r))
           (= (:fingerprint ledger) (:obligation-fingerprint r))
           (= (:transition-fingerprint ledger) (:transition-fingerprint r))
           (= (:subject-fingerprint ledger) (:subject-fingerprint r))
           (= :typed-structural-initial-consequence (:proof-rule r))
           (= :declared-abstract-state-projection-only (:scope r))
           (= :initial-condition-implies-invariant-for-all-declared-states
              (:claim r))
           (= success (:conditionally-checked-initial-ids r))
           (= open-ids (:unsupported-initial-ids r))
           (= (set (map #(vector :initial-state %) invariant-ids))
              (set/union success open-ids))
           (empty? (set/intersection success open-ids))
           (not (and (:initial-condition-manifestly-false? r) (seq success)))
           (= (:open-obligation-ids ledger)
              (:original-open-obligation-ids r))
           (= :restricted-conditional-initial-consequence-checked
              (:proof-status r))
           (= :not-established (:initial-state-existence r))
           (= :not-established (:initial-state-coverage r))
           (= :not-independently-proved (:checker-soundness r))
           (= :not-issued (:certificate r))
           (= :not-admitted (:admission r))
           (= :none (:authority r)))
      (reject! :initial-checker-contract-changed {}))
    by-id))

(defn- validate-preservation! [r ledger invariant-ids]
  (let [by-id (validate-outcomes! :preservation (:preservation-outcomes r)
                                  invariant-ids :preservation)
        success (set (map :obligation-id
                          (filter #(= checked (:status %))
                                  (:preservation-outcomes r))))
        open-ids (set (map :obligation-id
                           (filter #(= open (:status %))
                                   (:preservation-outcomes r))))]
    (when-not
      (and (= preservation/report-type (:type r))
           (= preservation/unified-preservation-version (:version r))
           (sha256? (:fingerprint r))
           (= (:fingerprint ledger) (:obligation-fingerprint r))
           (= (:transition-fingerprint ledger) (:transition-fingerprint r))
           (= (:subject-fingerprint ledger) (:subject-fingerprint r))
           (= :source-derived-union-of-whole-invariant-inferences
              (:proof-rule r))
           (= :declared-abstract-state-projection-only (:scope r))
           (= :all-pre-invariants-and-matching-transition-imply-post-invariant
              (:claim r))
           (= success (:conditionally-checked-preservation-ids r))
           (= open-ids (:unsupported-preservation-ids r))
           (= (set (map #(vector :preservation %) invariant-ids))
              (set/union success open-ids))
           (empty? (set/intersection success open-ids))
           (= (:open-obligation-ids ledger)
              (:original-open-obligation-ids r))
           (= :restricted-conditional-preservation-checked (:proof-status r))
           (= :not-independently-proved (:checker-soundness r))
           (= :not-issued (:certificate r))
           (= :not-admitted (:admission r))
           (= :none (:authority r)))
      (reject! :preservation-checker-contract-changed {}))
    by-id))

(defn check-induction!
  "Check a source-owned, nonempty abstract operation catalog against its own
   typed initial condition and invariants. One unsupported base case OR one
   unsupported preservation operation leaves the corresponding invariant OPEN.
   Even when every invariant is covered, return only CONDITIONAL abstract
   induction evidence; original obligations and physical coverage stay open."
  [catalog]
  (let [catalog-report (catalog-report! catalog)
        by-operation (validate-catalog! catalog catalog-report)
        invariant-ids (:invariant-ids catalog-report)
        ordered (vec (sort-by str (:declared-operation-ids catalog)))
        entries (into {} (map (juxt :id identity)) (:operations catalog))
        checks
        (mapv
         (fn [id]
           (let [source {:construction (:construction (get entries id))
                         :initial-condition (:initial-condition catalog)
                         :invariants (:invariants catalog)}
                 ledger (validate-ledger! source (get by-operation id)
                                          (source-ledger! source) invariant-ids)
                 base (initial-report! source)
                 step (preservation-report! source)]
             {:operation-id id
              :obligation-fingerprint (:fingerprint ledger)
              :transition-fingerprint (:transition-fingerprint ledger)
              :initial-fingerprint (:fingerprint base)
              :preservation-fingerprint (:fingerprint step)
              :original-open-obligation-ids (:open-obligation-ids ledger)
              :initial-outcomes (validate-initial! base ledger invariant-ids)
              :preservation-outcomes (validate-preservation! step ledger invariant-ids)
              :open-meta-obligations
              (set/union (:open-meta-obligations base)
                         (:open-meta-obligations step))}))
         ordered)
        first-base (:initial-outcomes (first checks))
        _ (doseq [entry (rest checks)]
            (when-not (= (into {} (map (fn [[id x]] [id (select-keys x [:status :rule :conjunct-path :reason])]))
                               first-base)
                         (into {} (map (fn [[id x]] [id (select-keys x [:status :rule :conjunct-path :reason])]))
                               (:initial-outcomes entry)))
              (reject! :shared-initial-inference-disagreement
                       {:operation-id (:operation-id entry)})))
        outcomes
        (mapv
         (fn [invariant-id]
           (let [base (get first-base invariant-id)
                 passed-base? (= checked (:status base))
                 preserving (set (for [e checks
                                       :when (= checked
                                                (get-in e [:preservation-outcomes invariant-id :status]))]
                                   (:operation-id e)))
                 missing (set/difference (set ordered) preserving)
                 closed? (and passed-base? (empty? missing))]
             {:invariant invariant-id
              :initial-obligation-id [:initial-state invariant-id]
              :initial-status (:status base)
              :initial-rule (:rule base)
              :preservation-obligation-id [:preservation invariant-id]
              :preserving-operation-ids preserving
              :unsupported-operation-ids missing
              :status (if closed?
                        :checked-conditional-induction
                        :open)
              :reason (cond
                        (not passed-base?) :initial-consequence-open
                        (seq missing) :some-preservation-open
                        :else nil)}))
         (sort-by str invariant-ids))
        supported (set (map :invariant
                            (filter #(= :checked-conditional-induction
                                        (:status %)) outcomes)))
        unsupported (set/difference invariant-ids supported)
        _ (when-not (and (= invariant-ids (set/union supported unsupported))
                         (empty? (set/intersection supported unsupported))
                         (= (count outcomes) (count invariant-ids)))
            (reject! :induction-invariant-coverage-lost {}))
        meta-obligations
        (reduce set/union
                (set/union (:open-meta-obligations catalog-report)
                           #{:induction-rule-soundness
                             :initial-state-existence
                             :external-initializer-coverage
                             :external-operation-set-completeness
                             :physical-write-path-coverage
                             :certificate-admission
                             :implementation-correspondence})
                (map :open-meta-obligations checks))
        source-ident (subject/describe-subject
                      {:kind :rule :id identity-id
                       :interface
                       {:version induction-version
                        :catalog-fingerprint (:fingerprint catalog-report)
                        :operation-evidence (mapv #(dissoc % :initial-outcomes
                                                             :preservation-outcomes
                                                             :open-meta-obligations)
                                                  checks)
                        :outcomes outcomes
                        :open-meta-obligations meta-obligations}})]
    {:type report-type
     :version induction-version
     :fingerprint (:fingerprint source-ident)
     :catalog-fingerprint (:fingerprint catalog-report)
     :catalog-id (:id catalog)
     :declared-operation-ids (:declared-operation-ids catalog)
     :declared-invariant-ids invariant-ids
     :operation-evidence
     (mapv #(dissoc % :initial-outcomes :preservation-outcomes) checks)
     :induction-outcomes outcomes
     :conditionally-checked-inductive-invariant-ids supported
     :unsupported-inductive-invariant-ids unsupported
     :all-declared-invariants-conditionally-inductive? (empty? unsupported)
     :original-open-obligation-ids-by-operation
     (:open-obligation-ids-by-operation catalog-report)
     :proof-rule :exact-base-and-all-declared-transitions
     :scope :closed-declared-abstract-machine-only
     :claim :conditional-induction-on-matching-declared-abstract-transitions
     :checking :original-operation-set-and-original-initial-and-preservation-sources
     :proof-status :restricted-conditional-induction-candidates-checked
     :open-meta-obligations meta-obligations
     :checker-soundness :not-independently-proved
     :initial-state-existence :not-established
     :external-initialization-coverage :not-established
     :external-operation-set-completeness :not-established
     :physical-write-path-coverage :not-established
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))
