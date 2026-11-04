(ns gesso.assurance.proof.initial-cases
  "Finite, SOURCE-OWNED literal initial-constructor catalog (v1274).

   (check-initial-cases! original-v1241-obligation-source
                         {:id :request/initial-constructors
                          :declared-constructor-ids #{:request/new :request/import}
                          :constructors [{:id :request/new :fields {...}}
                                         {:id :request/import :fields {...}}]})

   The catalog explicitly declares its expected constructor IDs and supplies
   exactly one v1272 typed TOTAL LITERAL constructor per ID. Every case is
   independently checked against the ORIGINAL typed initial condition and
   EVERY declared invariant. No case may be silently skipped. A false
   invariant remains a visible VIOLATION, never a successful initial theorem.

   The finite set is complete ONLY relative to the DECLARED catalog. The
   external assertion that every possible abstract/physical initializer is
   represented is not proved, and no implication over every state satisfying
   Init(s) follows from enumeration alone. A v1270 consequence proof may
   instead justify such an implication independently. This reports COPYABLE,
   conditional case evidence, not a certificate, initializer authorization,
   actual XTDB coverage, or independent proof of the checker itself."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.initial-constructor :as constructor]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.subject :as subject]))

(def initial-cases-version 1)
(def report-type :gesso.assurance.proof.initial-cases/unadmitted-initial-catalog)

(def ^:private identity-id :gesso.assurance.proof/initial-cases-v1)
(def ^:private max-constructors 32)
(def ^:private expected-obligation-categories
  #{:initial-state :preservation :assumption :implementation})
(def ^:private required-assumptions
  #{[:assumption :initial-state-existence]
    [:assumption :initial-state-coverage]
    [:assumption :checker-meta-soundness]
    [:assumption :declared-state-projection-coverage]
    [:assumption :declared-invariant-set-coverage]})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso finite initial-constructor catalog."
                  (merge {:error/type ::invalid-initial-cases
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [x expected at]
  (when-not (plain-map? x)
    (reject! :expected-plain-map at {}))
  (metadata-free! x at)
  (let [actual (set (keys x))]
    (when-not (= expected actual)
      (reject! :wrong-keys at
               {:missing (set/difference expected actual)
                :unexpected (set/difference actual expected)})))
  x)

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- sha256? [x]
  (and (string? x)
       (boolean (re-matches #"sha256:[0-9a-f]{64}" x))))

;; Capture the actual source-derived checkers. A replaced public Var is
;; cross-checked against fresh original results, not accepted as testimony.
;; This is not an independent soundness theorem for either implementation.
(def ^:private source-obligation-checker obligation/derive-obligations!)
(def ^:private source-case-checker constructor/check-initial-constructor!)

(defn- source-obligations! [source]
  (let [public (var-get #'obligation/derive-obligations!)]
    (if (identical? public source-obligation-checker)
      (source-obligation-checker source)
      (let [reported (public source)
            expected (source-obligation-checker source)]
        (when-not (= reported expected)
          (reject! :obligation-checker-contract-changed [] {}))
        expected))))

(defn- source-case! [source case]
  (let [public (var-get #'constructor/check-initial-constructor!)]
    (if (identical? public source-case-checker)
      (source-case-checker source case)
      (let [reported (public source case)
            expected (source-case-checker source case)]
        (when-not (= reported expected)
          (reject! :constructor-checker-contract-changed
                   [:constructors (:id case)] {}))
        expected))))

(defn- checked-ledger! [source]
  (let [ledger (source-obligations! source)
        categories (:obligations ledger)
        initial (get categories :initial-state)
        invariant-ids (set (keys (:invariants ledger)))
        expected-ids (set (map #(vector :initial-state %) invariant-ids))
        all-obligations (mapcat val categories)
        all-ids (map :id all-obligations)]
    (when-not (and (= obligation/obligation-type (:type ledger))
                   (= obligation/obligation-version (:version ledger))
                   (= source (:source ledger))
                   (sha256? (:fingerprint ledger))
                   (sha256? (:transition-fingerprint ledger))
                   (sha256? (:subject-fingerprint ledger))
                   (= expected-obligation-categories (set (keys categories)))
                   (map? (:invariants ledger))
                   (seq (:invariants ledger))
                   (vector? initial)
                   (= (count expected-ids) (count initial))
                   (= expected-ids (set (map :id initial)))
                   (every? #(and (= :initial-state (:kind %))
                                 (= :open (:status %))) initial)
                   (= (count all-ids) (count (set all-ids)))
                   (= (set all-ids) (:open-obligation-ids ledger))
                   (set/subset? required-assumptions (:open-obligation-ids ledger))
                   (every? #(= :open (:status %)) all-obligations)
                   (= :all-obligations-open (:status ledger))
                   (= :not-proved (:proof-status ledger))
                   (= :not-issued (:certificate ledger))
                   (= :not-admitted (:admission ledger))
                   (= :not-established (:runtime-enforcement ledger))
                   (= :not-established (:implementation-correspondence ledger))
                   (= :none (:authority ledger)))
      (reject! :obligation-source-contract-changed [] {}))
    {:ledger ledger :initial-ids expected-ids}))

(defn- checked-catalog! [catalog]
  (exact-map! catalog #{:id :declared-constructor-ids :constructors} [:catalog])
  (when-not (qualified-id? (:id catalog))
    (reject! :invalid-catalog-id [:catalog :id] {}))
  (let [declared (:declared-constructor-ids catalog)
        constructors (:constructors catalog)]
    (when-not (and (set? declared) (seq declared)
                   (<= (count declared) max-constructors)
                   (every? qualified-id? declared))
      (reject! :invalid-declared-constructor-ids
               [:catalog :declared-constructor-ids]
               {:max-constructors max-constructors}))
    (metadata-free! declared [:catalog :declared-constructor-ids])
    (when-not (and (vector? constructors)
                   (instance? clojure.lang.APersistentVector constructors)
                   (seq constructors)
                   (<= (count constructors) max-constructors))
      (reject! :invalid-constructor-sequence
               [:catalog :constructors]
               {:max-constructors max-constructors}))
    (metadata-free! constructors [:catalog :constructors])
    (doseq [[index case] (map-indexed vector constructors)]
      (exact-map! case #{:id :fields} [:catalog :constructors index])
      (when-not (qualified-id? (:id case))
        (reject! :invalid-constructor-id
                 [:catalog :constructors index :id] {})))
    (let [ids (mapv :id constructors)
          actual (set ids)]
      (when-not (= (count ids) (count actual))
        (reject! :duplicate-constructor-id [:catalog :constructors]
                 {:duplicates (set (for [[id n] (frequencies ids)
                                         :when (> n 1)] id))}))
      (when-not (= actual declared)
        (reject! :constructor-id-coverage-mismatch [:catalog :constructors]
                 {:missing (set/difference declared actual)
                  :undeclared (set/difference actual declared)}))
      ;; A catalog is a SET of cases: input vector order cannot change its
      ;; semantics or fingerprint. IDs are qualified keywords.
      (vec (sort-by (comp str :id) constructors)))))

(defn- validate-case-report! [r case ledger expected]
  (let [outcomes (:initial-case-outcomes r)
        checked (:checked-initial-case-ids r)
        violated (:violated-initial-case-ids r)
        ids (mapv :initial-obligation-id outcomes)]
    (when-not
      (and (= constructor/report-type (:type r))
           (= constructor/initial-constructor-version (:version r))
           (sha256? (:fingerprint r))
           (= (:id case) (:constructor-id r))
           (= (:fingerprint ledger) (:obligation-fingerprint r))
           (= (:transition-fingerprint ledger) (:transition-fingerprint r))
           (= (:subject-fingerprint ledger) (:subject-fingerprint r))
           (= true (:initial-condition-holds? r))
           (vector? outcomes)
           (= (count expected) (count outcomes))
           (= expected (set ids))
           (= expected (set/union checked violated))
           (empty? (set/intersection checked violated))
           (every? (fn [{:keys [invariant initial-obligation-id status value]}]
                     (and (= [:initial-state invariant] initial-obligation-id)
                          (= (if value :checked-constructor-case
                                 :violated-constructor-case)
                             status)
                          (instance? Boolean value)))
                   outcomes)
           (= (empty? violated) (:all-declared-invariants-hold? r))
           (= (:open-obligation-ids ledger) (:original-open-obligation-ids r))
           (= :source-derived-conditional-initial-case-check (:proof-status r))
           (= :not-established (:initial-case-catalog-completeness r))
           (= :not-established (:physical-initialization-coverage r))
           (= :not-independently-proved (:checker-soundness r))
           (= :not-issued (:certificate r))
           (= :not-admitted (:admission r))
           (= :not-established (:runtime-enforcement r))
           (= :not-established (:implementation-correspondence r))
           (= :none (:authority r)))
      (reject! :constructor-report-contract-changed
               [:catalog :constructors (:id case)] {}))
    r))

(defn check-initial-cases!
  "Recheck every constructor in a finite SOURCE-OWNED catalog against original
   typed initial-state and invariant declarations. The declared ID set is
   matched exactly, not inferred from a potentially incomplete branch list.

   Returns per-constructor evidence and a per-invariant aggregation. An
   invariant is 'checked-across-declared-cases' only if EVERY declared case
   passes it. All original obligations remain OPEN: in particular, neither
   physical initializer coverage nor completeness of the declared finite
   constructor set is established by this enumeration itself."
  [obligation-source catalog]
  (let [cases (checked-catalog! catalog)
        {:keys [ledger initial-ids]} (checked-ledger! obligation-source)
        checked-reports (mapv (fn [case]
                                (validate-case-report!
                                 (source-case! obligation-source case)
                                 case ledger initial-ids))
                              cases)
        case-results (mapv (fn [r]
                             {:constructor-id (:constructor-id r)
                              :constructor-fingerprint (:fingerprint r)
                              :constructed-state (:constructed-state r)
                              :checked-initial-case-ids
                              (:checked-initial-case-ids r)
                              :violated-initial-case-ids
                              (:violated-initial-case-ids r)})
                           checked-reports)
        checked (reduce set/intersection
                        (map :checked-initial-case-ids case-results))
        violated (set/difference initial-ids checked)
        outcomes (mapv (fn [id]
                         (let [failing (set (for [c case-results
                                                  :when (contains?
                                                         (:violated-initial-case-ids c)
                                                         id)]
                                              (:constructor-id c)))]
                           {:initial-obligation-id id
                            :invariant (second id)
                            :status (if (empty? failing)
                                      :checked-across-declared-cases
                                      :violated-declared-case)
                            :violating-constructor-ids failing}))
                       (sort-by str initial-ids))
        _ (when-not (and (= initial-ids (set/union checked violated))
                         (empty? (set/intersection checked violated))
                         (= (count outcomes) (count initial-ids)))
            (reject! :initial-catalog-coverage-lost [:catalog] {}))
        identity (subject/describe-subject
                  {:kind :rule :id identity-id
                   :interface {:version initial-cases-version
                               :catalog-id (:id catalog)
                               :declared-constructor-ids
                               (:declared-constructor-ids catalog)
                               :obligation-fingerprint (:fingerprint ledger)
                               :cases (mapv (fn [case r]
                                              {:constructor case
                                               :result-fingerprint (:fingerprint r)})
                                            cases checked-reports)
                               :outcomes outcomes}})]
    {:type report-type
     :version initial-cases-version
     :fingerprint (:fingerprint identity)
     :catalog-id (:id catalog)
     :declared-constructor-ids (:declared-constructor-ids catalog)
     :obligation-fingerprint (:fingerprint ledger)
     :transition-fingerprint (:transition-fingerprint ledger)
     :subject-fingerprint (:subject-fingerprint ledger)
     :declared-id-coverage :exact
     :case-results case-results
     :initial-catalog-outcomes outcomes
     :checked-declared-case-initial-ids checked
     :violated-declared-case-initial-ids violated
     :all-declared-cases-satisfy-invariants? (empty? violated)
     :original-open-obligation-ids (:open-obligation-ids ledger)
     :checking :original-declared-constructors-and-original-typed-rules
     :proof-rule :finite-literal-initial-case-enumeration
     :scope :finite-declared-literal-constructor-cases-only
     :claim :every-declared-constructor-case-evaluated
     :proof-status :conditional-declared-initial-case-coverage-checked
     :external-constructor-catalog-completeness :not-established
     :physical-initialization-coverage :not-established
     :checker-soundness :not-independently-proved
     :open-meta-obligations
     #{:external-constructor-catalog-completeness
       :physical-initialization-coverage
       :initial-rule-evaluation-soundness
       :certificate-admission}
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))
