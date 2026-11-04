(ns gesso.assurance.inductive-report
  "Source-bound, COPYABLE conditional abstract safety report (v1286).

   This report joins the original-source induction ledger, the exact v1282
   dependency-closure handle, and the v1280 in-process induction certificate.
   Its *checked* mode requires BOTH live handles to be current against the
   SAME original catalog and dependency graph. A merely admitted generic DAG
   is insufficient: the source-relative closure checker must also succeed.

   A source-only mode exposes incomplete induction without inventing a
   certificate. Report maps and fingerprints are NOT capabilities. Even with
   both handles, all original obligations and physical, initialization, and
   metatheoretic assumptions remain OPEN. Nothing here attests complete
   deployed write paths, XTDB refinement, checker soundness or authority.

   Trust boundary: same trusted JVM, proof rules and handle registries. No
   security claim against arbitrary malicious changes to that JVM is made."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction-dependency :as induction-dependency]
            [gesso.assurance.proof.induction-ledger :as ledger]
            [gesso.assurance.subject :as subject]))

(def report-version 1)
(def report-type :gesso.assurance.inductive-report/source-bound-abstract-safety)
(def ^:private report-id :gesso.assurance/inductive-safety-report-v1)
(def ^:private source-keys
  #{:catalog :certificate-handle :dependency-handle :current-graph})
(def ^:private hash-pattern #"sha256:[0-9a-f]{64}")
(def ^:private induction-premise
  :gesso.assurance.proof.induction/matching-declared-abstract-machine)
(def ^:private unavoidable-open
  #{:induction-rule-soundness
    :initial-state-existence
    :external-initializer-coverage
    :external-operation-set-completeness
    :physical-write-path-coverage
    :implementation-correspondence
    :checker-meta-soundness})

(defn- reject! [reason details]
  (throw (ex-info "Invalid Gesso source-bound inductive assurance report."
                  (merge {:error/type ::invalid-inductive-report
                          :reason reason}
                         details))))

(defn- hash? [x]
  (and (string? x) (boolean (re-matches hash-pattern x))))

(defn- strict-input! [source]
  (when-not (and (map? source) (not (record? source))
                 (nil? (meta source))
                 (= source-keys (set (keys source))))
    (reject! :invalid-source-input
             {:required-keys source-keys}))
  (let [{:keys [certificate-handle dependency-handle current-graph]} source
        supplied (mapv some? [certificate-handle dependency-handle current-graph])]
    (cond
      (every? false? supplied) :source-only
      (every? true? supplied) :checked-handles
      :else (reject! :incomplete-evidence-package
                     {:required #{:certificate-handle :dependency-handle
                                  :current-graph}}))))

(defn- require-ledger-shape! [l]
  (when-not
    (and (= ledger/ledger-type (:type l))
         (= ledger/ledger-version (:version l))
         (hash? (:fingerprint l))
         (hash? (:catalog-fingerprint l))
         (hash? (:induction-fingerprint l))
         (set? (:declared-operation-ids l))
         (set? (:declared-invariant-ids l))
         (seq (:declared-operation-ids l))
         (seq (:declared-invariant-ids l))
         (= (:declared-invariant-ids l)
            (set (map :invariant-id
                      (concat (:checked-abstract-steps l)
                              (:open-abstract-steps l)))))
         (= (:declared-operation-ids l)
            (set (keys (:original-open-obligation-ids-by-operation l))))
         (= :all-original-obligations-remain-open
            (:original-obligation-status l))
         (set? (:open-meta-obligations l))
         (set/subset? unavoidable-open (:open-meta-obligations l))
         (= :not-performed (:assumption-discharge l))
         (= :closed-declared-abstract-machine-only (:scope l))
         (= :none (:authority l)))
    (reject! :induction-ledger-contract-changed {}))
  l)

(defn- require-bound-evidence!
  [catalog current-graph certificate-handle dependency-handle l]
  ;; The two live checkers authenticate their own handle registries and
  ;; re-evaluate ORIGINAL sources. Checking both against the SAME graph joins
  ;; generic certificate admission to exact induction dependency closure.
  (induction-dependency/require-current-induction-dependencies!
   dependency-handle catalog current-graph)
  (certificate/require-current-certificate!
   certificate-handle catalog current-graph)
  (let [d (induction-dependency/describe-induction-dependencies
           dependency-handle)
        c (certificate/describe-certificate certificate-handle)
        operations (:declared-operation-ids l)
        invariants (:declared-invariant-ids l)
        inherited (:original-open-obligation-ids-by-operation l)]
    (when-not
      (and (true? (:all-declared-invariants-conditionally-inductive? l))
           (empty? (:open-abstract-steps l))
           (= :checked-conditional-abstract-induction-evidence
              (:proof-status l))
           (= induction-dependency/dependency-type (:type d))
           (= induction-dependency/dependency-version (:version d))
           (= :source-dependencies-structurally-checked (:proof-status d))
           (= :original-proof-source-and-exact-dependency-graph
              (:checking d))
           (hash? (:fingerprint d))
           (= (:catalog-id l) (:catalog-id d))
           (= (:catalog-fingerprint l) (:catalog-fingerprint d))
           (= (:induction-fingerprint l) (:induction-fingerprint d))
           (= operations (:operation-ids d))
           (= invariants (:invariant-ids d))
           (= inherited (:original-open-obligation-ids-by-operation d))
           (= :not-performed (:assumption-discharge d))
           (= :none (:authority d))
           (= certificate/certificate-type (:type c))
           (= certificate/certificate-version (:version c))
           (= :conditional-induction (:proof-kind c))
           (= :closed-declared-abstract-machine-only (:scope c))
           (= :source-relative-abstract-proof-only (:certificate-scope c))
           (= :checked-conditional-abstract-induction (:proof-status c))
           (= :in-process-checker-backed (:admission c))
           (= :original-source-independently-rechecked (:checking c))
           (hash? (:fingerprint c))
           (= (:induction-fingerprint l) (:derivation-fingerprint c))
           (= (:catalog-id l) (get-in c [:goal :catalog-id]))
           (= invariants (get-in c [:goal :invariant-ids]))
           (= #{induction-premise} (:open-premises c))
           (set? (:open-obligations c))
           (set/subset? (disj (:open-meta-obligations l)
                              :certificate-admission)
                        (:open-obligations c))
           (set/subset? unavoidable-open
                        (set/union (:open-obligations c)
                                   (:open-external-obligations d)))
           (= :not-performed (:assumption-discharge c))
           (= :not-established (:meta-soundness c))
           (= :not-established (:implementation-correspondence c))
           (= :not-established (:runtime-enforcement c))
           (= :none (:authority c)))
      (reject! :mismatched-source-bound-evidence
               {:catalog-id (:catalog-id l)}))
    {:certificate c :dependency d}))

(defn build-inductive-report!
  "Rebuild a source-relative safety explanation from ORIGINAL source.

   Input has exactly:
     {:catalog <original operation catalog>
      :certificate-handle <in-process v1280 handle or nil>
      :dependency-handle <in-process v1282 closure handle or nil>
      :current-graph <live full dependency DAG or nil>}

   All three evidence inputs must be present together or all nil. When
   supplied, both handles must be CURRENT against this same catalog and DAG.
   The report always keeps the ENTIRE original obligation register and ALL
   open assumptions; a successful conditional induction is never universal
   implementation assurance. In source-only mode it describes partial or
   complete unadmitted inference without silently accepting fake evidence.

   Never supply a copied checker report as :catalog. Nothing returned here
   is a token, execution permission, or production certificate."
  [source]
  (let [mode (strict-input! source)
        {:keys [catalog certificate-handle dependency-handle current-graph]}
        source
        l (require-ledger-shape! (ledger/check-induction-ledger! catalog))
        bound (when (= :checked-handles mode)
                (require-bound-evidence! catalog current-graph
                                         certificate-handle dependency-handle l))
        c (:certificate bound)
        d (:dependency bound)
        assumptions (set/union (:open-meta-obligations l)
                               (or (:open-obligations c) #{})
                               (or (:open-external-obligations d) #{}))
        open-premises (or (:open-premises c) #{})
        ;; The ledger classifies induction's ORIGINAL meta-obligations. The
        ;; certificate can add independent execution/transaction boundaries;
        ;; keep those OPEN in a disjoint supplemental classification instead
        ;; of silently omitting them from the category summary.
        all-open-classes
        (assoc (:open-assumptions l) :additional-boundaries
               (set/difference assumptions (:open-meta-obligations l)))
        _ (when-not (and (= assumptions
                            (reduce set/union #{} (vals all-open-classes)))
                         (= (count assumptions)
                            (reduce + (map count (vals all-open-classes)))))
            (reject! :assumption-classification-incomplete {}))
        checked-invariants (set (map :invariant-id
                                     (:checked-abstract-steps l)))
        open-invariants (set (map :invariant-id (:open-abstract-steps l)))
        evidence {:catalog-id (:catalog-id l)
                  :catalog-fingerprint (:catalog-fingerprint l)
                  :induction-fingerprint (:induction-fingerprint l)
                  :ledger-fingerprint (:fingerprint l)
                  :certificate-fingerprint (:fingerprint c)
                  :dependency-fingerprint (:fingerprint d)
                  :declared-operation-ids (:declared-operation-ids l)
                  :declared-invariant-ids (:declared-invariant-ids l)
                  :conditionally-checked-invariant-ids checked-invariants
                  :open-invariant-ids open-invariants
                  :exact-source-dependencies (if bound
                                               :checked :not-established)
                  :original-open-obligation-ids-by-operation
                  (:original-open-obligation-ids-by-operation l)
                  :open-assumption-classes all-open-classes
                  :open-assumptions assumptions
                  :open-premises open-premises
                  :checked-certificate? (boolean bound)}
        identity (subject/describe-subject
                  {:kind :rule :id report-id
                   :interface {:report-version report-version
                               :evidence evidence}})]
    {:type report-type
     :version report-version
     :fingerprint (:fingerprint identity)
     :checking (if bound :original-sources-and-both-live-handles-rechecked
                   :original-source-induction-rechecked)
     :catalog-id (:catalog-id l)
     :identity (select-keys evidence
                            [:catalog-fingerprint :induction-fingerprint
                             :ledger-fingerprint :certificate-fingerprint
                             :dependency-fingerprint])
     :declared-operation-ids (:declared-operation-ids l)
     :declared-invariant-ids (:declared-invariant-ids l)
     :classification
     {:checked-abstract {:status (if bound
                                   :source-bound-conditional-induction
                                   (:proof-status l))
                         :invariant-ids checked-invariants
                         :steps (:checked-abstract-steps l)
                         :scope :closed-declared-abstract-machine-only}
      :open-abstract {:invariant-ids open-invariants
                      :steps (:open-abstract-steps l)}
      :certificate {:status (if bound
                              :in-process-conditional-abstract-certificate
                              :not-admitted)
                    :fingerprint (:fingerprint c)
                    :open-premises open-premises}
      :dependency-closure {:status (if bound
                                     :exact-source-relative-closure-checked
                                     :not-established)
                           :fingerprint (:fingerprint d)}
      :original-obligations {:status :all-original-obligations-remain-open
                             :by-operation (:original-open-obligation-ids-by-operation l)}
      :open {:assumptions assumptions
             :assumption-classes all-open-classes
             :certificate-premises open-premises
             :logical-soundness :not-independently-proved
             :initialization-coverage :not-established
             :physical-operation-coverage :not-established
             :implementation-correspondence :not-established}
      :unsupported {:production-certification :not-established
                    :runtime-authorization :not-established
                    :universal-xtdb-correspondence :not-established}}
     :all-declared-invariants-conditionally-inductive?
     (:all-declared-invariants-conditionally-inductive? l)
     :source-dependency-closure (if bound :checked-exact-source-relative
                                  :not-established)
     :assumption-discharge :not-performed
     :verdict (if bound :source-bound-conditional-abstract-safety
                  :source-only-conditional-abstract-analysis)
     :scope :closed-declared-abstract-machine-only
     :full-success? false
     :production-certification :not-established
     :implementation-correspondence :not-established
     :runtime-enforcement :not-established
     :transaction-authority :none
     :authority :none}))

(defn require-current-inductive-report!
  "Reject a copied/altered/stale report against CURRENT original source and
   live handles. Returns a fresh COPYABLE report, never an admission handle."
  [prior current-source]
  (let [current (build-inductive-report! current-source)]
    (when-not (and (map? prior) (not (record? prior))
                   (nil? (meta prior)) (= prior current))
      (reject! :stale-or-forged-inductive-report
               {:expected (:fingerprint current)
                :actual (:fingerprint prior)}))
    current))
