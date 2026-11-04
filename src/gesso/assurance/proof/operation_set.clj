(ns gesso.assurance.proof.operation-set
  "Closed DECLARED set of original Gesso abstract operations (v1276).

   (check-operation-set!
    {:id :request/operation-catalog
     :declared-operation-ids #{:request/claim :request/unclaim}
     :initial-condition <one typed state predicate>
     :invariants {<qualified invariant id> <typed state predicate> ...}
     :operations [{:id :request/claim :construction <v1239 original source>}
                  {:id :request/unclaim :construction <v1239 original source>} ]})

   The catalog owns the SAME initial-condition and invariants for every
   operation; it does not ask the application to duplicate its business rules
   in per-operation proof inputs. Every original construction is rechecked by
   v1241's existing obligation generator, including exact typed read/write/
   frame relationships, guard, assignment, subject, and source-derived
   obligations. The catalog rejects missing, extra, duplicated, or misbound
   operation IDs; operations with different state projections or nominal state
   types cannot be composed into this abstract machine.

   The set is closed only RELATIVE TO ITS DECLARATION. It is NOT evidence that
   every physical writer, route, dynamic operation, initialization path, or
   effect is represented. An unchecked real operation could invalidate any
   future inductive theorem. No preservation result, certificate, production
   correspondence, or implementation authority is issued here. The resulting
   fingerprints are copyable semantic change detectors, not credentials."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.subject :as subject]))

(def operation-set-version 1)
(def report-type :gesso.assurance.proof.operation-set/unadmitted-operation-set)

(def ^:private identity-id :gesso.assurance.proof/operation-set-v1)
(def ^:private max-operations 32)
(def ^:private categories
  #{:initial-state :preservation :assumption :implementation})
(def ^:private required-assumptions
  #{[:assumption :initial-state-existence]
    [:assumption :initial-state-coverage]
    [:assumption :declared-state-projection-coverage]
    [:assumption :declared-invariant-set-coverage]
    [:assumption :checker-meta-soundness]})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso source-owned abstract operation set."
                  (merge {:error/type ::invalid-operation-set
                          :reason reason :at at} details))))

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
  (and (string? x) (boolean (re-matches #"sha256:[0-9a-f]{64}" x))))

;; Preserve a source-derived reference implementation when the public Var is
;; replaced. This defends against with-redefs-style sibling report injection,
;; not malicious replacement of compiled code or defects in the shared kernel.
(def ^:private original-obligation-checker obligation/derive-obligations!)

(defn- source-obligations! [source id]
  (let [public (var-get #'obligation/derive-obligations!)]
    (if (identical? public original-obligation-checker)
      (original-obligation-checker source)
      (let [reported (public source)
            expected (original-obligation-checker source)]
        (when-not (= expected reported)
          (reject! :obligation-checker-contract-changed
                   [:operations id] {}))
        expected))))

(defn- checked-catalog! [catalog]
  (exact-map! catalog
              #{:id :declared-operation-ids :initial-condition
                :invariants :operations} [])
  (when-not (qualified-id? (:id catalog))
    (reject! :invalid-catalog-id [:id] {}))
  (let [declared (:declared-operation-ids catalog)
        operations (:operations catalog)]
    (when-not (and (instance? clojure.lang.PersistentHashSet declared)
                   (seq declared)
                   (<= (count declared) max-operations)
                   (every? qualified-id? declared))
      (reject! :invalid-declared-operation-ids [:declared-operation-ids]
               {:max-operations max-operations}))
    (metadata-free! declared [:declared-operation-ids])
    (when-not (and (instance? clojure.lang.APersistentVector operations)
                   (seq operations)
                   (<= (count operations) max-operations))
      (reject! :invalid-operation-sequence [:operations]
               {:max-operations max-operations}))
    (metadata-free! operations [:operations])
    (doseq [[index entry] (map-indexed vector operations)]
      (exact-map! entry #{:id :construction} [:operations index])
      (when-not (qualified-id? (:id entry))
        (reject! :invalid-operation-id [:operations index :id] {}))
      (when-not (plain-map? (:construction entry))
        (reject! :expected-construction-source
                 [:operations index :construction] {}))
      ;; Even an otherwise well-formed transition must be bound to the same
      ;; operation ID named by its catalog entry. No aliases or indirection.
      (when-not (= (:id entry)
                   (get-in entry [:construction :subject :id]))
        (reject! :operation-subject-id-mismatch [:operations index]
                 {:operation-id (:id entry)
                  :subject-id (get-in entry [:construction :subject :id])})))
    (let [ids (mapv :id operations)
          actual (set ids)]
      (when-not (= (count ids) (count actual))
        (reject! :duplicate-operation-id [:operations]
                 {:duplicates (set (for [[id n] (frequencies ids)
                                         :when (> n 1)] id))}))
      (when-not (= actual declared)
        (reject! :operation-id-coverage-mismatch [:operations]
                 {:missing (set/difference declared actual)
                  :undeclared (set/difference actual declared)}))
      ;; Operation order is not transition order: this is a set of distinct
      ;; possible transitions, NOT a sequential composition of operations.
      (vec (sort-by (comp str :id) operations)))))

(defn- check-ledger! [source id]
  (let [r (source-obligations! source id)
        construction (:construction source)
        obligations (:obligations r)
        all (mapcat val obligations)
        ids (mapv :id all)
        invariant-ids (set (keys (:invariants r)))
        initial-ids (set (map #(vector :initial-state %) invariant-ids))
        preservation-ids (set (map #(vector :preservation %) invariant-ids))]
    (when-not
      (and (= obligation/obligation-type (:type r))
           (= obligation/obligation-version (:version r))
           (= source (:source r))
           (sha256? (:fingerprint r))
           (sha256? (:transition-fingerprint r))
           (sha256? (:subject-fingerprint r))
           (= categories (set (keys obligations)))
           (= :operation (get-in construction [:subject :kind]))
           (= id (get-in construction [:subject :id]))
           (map? (:invariants r)) (seq (:invariants r))
           (every? vector? (vals obligations))
           (= initial-ids (set (map :id (:initial-state obligations))))
           (= preservation-ids (set (map :id (:preservation obligations))))
           (= (count invariant-ids) (count (:initial-state obligations)))
           (= (count invariant-ids) (count (:preservation obligations)))
           (= (count ids) (count (set ids)))
           (= (set ids) (:open-obligation-ids r))
           (set/subset? required-assumptions (:open-obligation-ids r))
           (every? #(= :open (:status %)) all)
           (= :all-obligations-open (:status r))
           (= :not-proved (:proof-status r))
           (= :not-issued (:certificate r))
           (= :not-admitted (:admission r))
           (= :not-established (:runtime-enforcement r))
           (= :not-established (:implementation-correspondence r))
           (= :none (:authority r)))
      (reject! :obligation-source-contract-changed [:operations id] {}))
    r))

(defn check-operation-set!
  "Validate an exact CLOSED DECLARED set of original, typed guarded
   transition sources against ONE shared initial rule and invariant map.

   Returns a deterministic source-bound catalog with the full exact sets of
   per-operation obligation IDs and original source-derived fingerprints. A
   change to any registered guard, assignment, subject, read/write/frame
   footprint, shared invariant, initial rule, or operation membership changes
   catalog identity. The report proves NEITHER invariant preservation NOR
   completeness of the real-world operation/effect universe."
  [catalog]
  (let [entries (checked-catalog! catalog)
        checked (mapv (fn [{:keys [id construction]}]
                        (let [source {:construction construction
                                      :initial-condition (:initial-condition catalog)
                                      :invariants (:invariants catalog)}]
                          {:id id
                           :construction construction
                           :ledger (check-ledger! source id)}))
                      entries)
        canonical-state-types (get-in (first checked) [:construction :state-types])
        canonical-state-projection (set (keys canonical-state-types))
        canonical-initial (get-in (first checked) [:ledger :initial-condition])
        canonical-invariants (get-in (first checked) [:ledger :invariants])
        invariant-ids (set (keys canonical-invariants))]
    (doseq [{:keys [id construction ledger]} checked]
      (when-not (= canonical-state-types (:state-types construction))
        (reject! :state-type-projection-mismatch [:operations id :construction :state-types]
                 {:expected-fields canonical-state-projection
                  :actual-fields (set (keys (:state-types construction)))}))
      (when-not (= canonical-initial (:initial-condition ledger))
        (reject! :initial-rule-disagreement [:operations id] {}))
      (when-not (= canonical-invariants (:invariants ledger))
        (reject! :invariant-rule-disagreement [:operations id] {})))
    (let [results
          (mapv (fn [{:keys [id construction ledger]}]
                  {:operation-id id
                   :obligation-fingerprint (:fingerprint ledger)
                   :transition-fingerprint (:transition-fingerprint ledger)
                   :subject-fingerprint (:subject-fingerprint ledger)
                   :reads (:reads construction)
                   :writes (:writes construction)
                   :frame (:frame construction)
                   :open-obligation-ids (:open-obligation-ids ledger)})
                checked)
          operation-ids (set (map :operation-id results))
          expected-obligation-ids (into {} (map (juxt :operation-id :open-obligation-ids) results))
          identity (subject/describe-subject
                    {:kind :rule
                     :id identity-id
                     :interface
                     {:version operation-set-version
                      :catalog-id (:id catalog)
                      :declared-operation-ids (:declared-operation-ids catalog)
                      :state-types canonical-state-types
                      :initial-condition canonical-initial
                      :invariants canonical-invariants
                      :operation-results results}})]
      (when-not (= operation-ids (:declared-operation-ids catalog))
        (reject! :operation-set-coverage-lost [] {}))
      {:type report-type
       :version operation-set-version
       :fingerprint (:fingerprint identity)
       :catalog-id (:id catalog)
       :declared-operation-ids (:declared-operation-ids catalog)
       :exact-declared-operation-coverage :checked
       :state-projection canonical-state-projection
       :invariant-ids invariant-ids
       :operation-results results
       :open-obligation-ids-by-operation expected-obligation-ids
       :checking :original-typed-operation-sources-and-shared-rules
       :proof-status :declared-abstract-operation-set-checked
       :scope :closed-declared-abstract-operation-set-only
       :external-operation-set-completeness :not-established
       :physical-write-path-coverage :not-established
       :initialization-coverage :not-established
       :invariant-preservation :not-established
       :checker-soundness :not-independently-proved
       :open-meta-obligations
       #{:external-operation-set-completeness
         :physical-write-path-coverage
         :initialization-coverage
         :abstract-invariant-preservation
         :checker-meta-soundness
         :certificate-admission}
       :certificate :not-issued
       :admission :not-admitted
       :runtime-enforcement :not-established
       :implementation-correspondence :not-established
       :authority :none})))
