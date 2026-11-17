(ns gesso.assurance.proof.operation-interface-composition
  "v1443: source-derived conditional composition of operation interfaces.

   The composition represents ALTERNATIVE operations over one shared abstract
   state, not a sequential atomic transaction. It requires the complete live
   v1440 application dependency closure, rederives v1442 interfaces from the
   original catalog, and refuses incompatible state schemas. Per-operation
   input types, guard/assignment contracts, invariant results, and all
   original open obligations remain individually visible.

   Interactions describe potential read/write interference, NOT an assertion
   that such operations execute concurrently, are serialized, or commute.
   No certificate, physical writer coverage, or runtime admission is issued."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.operation-interface :as iface]
            [gesso.assurance.subject :as subject]))

(def composition-version 1)
(def composition-type
  :gesso.assurance.proof.operation-interface-composition/conditional-abstract-catalog)
(def ^:private identity-id
  :gesso.assurance.proof/conditional-operation-interface-composition-v1)

(defn- reject! [reason details]
  (throw (ex-info "Invalid Gesso operation-interface composition."
                  (merge {:error/type ::invalid-operation-interface-composition
                          :reason reason} details))))

(defn- sorted-ids [ids]
  (sort-by str ids))

(defn- sorted-map* [pairs]
  (into (sorted-map-by #(compare (pr-str %1) (pr-str %2))) pairs))

(defn- interfaces! [derived]
  (let [{:keys [interfaces operation-ids invariant-ids
                original-open-obligation-ids-by-operation
                open-meta-obligations]} derived]
    (when-not (and (map? interfaces) (seq operation-ids)
                   (= operation-ids (set (keys interfaces)))
                   (= operation-ids
                      (set (keys original-open-obligation-ids-by-operation)))
                   (set? invariant-ids)
                   (set? open-meta-obligations))
      (reject! :incomplete-operation-or-premise-catalog {}))
    (doseq [id operation-ids]
      (let [{:keys [state-types input-types reads writes frame
                    conditional-invariant-results
                    original-open-obligation-ids open-meta-obligations] :as op}
            (get interfaces id)
            state-ids (set (keys state-types))]
        (when-not (and (= id (:operation-id op))
                       (= (:catalog-id derived) (:catalog-id op))
                       (= (:catalog-fingerprint derived)
                          (:catalog-fingerprint op))
                       (map? state-types) (seq state-types)
                       (map? input-types)
                       (set? reads) (set? writes) (set? frame)
                       (= state-ids (set/union writes frame))
                       (empty? (set/intersection writes frame))
                       (set/subset? writes state-ids)
                       (set/subset? reads
                                    (set/union state-ids (set (keys input-types))))
                       (= invariant-ids (set (keys conditional-invariant-results)))
                       (= original-open-obligation-ids
                          (get original-open-obligation-ids-by-operation id))
                       (= open-meta-obligations (:open-meta-obligations derived))
                       (= :not-issued (:certificate op))
                       (= :not-admitted (:admission op))
                       (= :none (:authority op)))
          (reject! :invalid-source-operation-interface {:operation-id id}))))
    interfaces))

(defn- common-state-types! [interfaces]
  ;; A single-machine interface has one complete state schema. Compatible
  ;; subsets might eventually be composable using an independently checked
  ;; frame-extension rule; v1443 does NOT manufacture that rule.
  (let [ids (sorted-ids (keys interfaces))
        first-id (first ids)
        expected (get-in interfaces [first-id :state-types])]
    (doseq [id (rest ids)]
      (when-not (= expected (get-in interfaces [id :state-types]))
        (reject! :incompatible-shared-state-schema
                 {:left first-id :right id})))
    expected))

(defn- aggregate-invariant-entry
  "Pure per-invariant aggregation descriptor, not a proved induction step.
   The caller must FIRST establish common initial, induction, and open-reason
   statuses across the original per-operation results and verify that overall
   conditional induction agrees with all of their preservation steps."
  [initial-status preservation-by-operation induction-status open-reason]
  {:initial-status initial-status
   :preservation-by-operation preservation-by-operation
   :induction-status induction-status
   :open-reason open-reason})

(defn- empty-preservation-statuses
  "Preserve the same deterministic sorted-map comparator as the source
   aggregator. The Typed Clojure checker audits this initial map source."
  []
  (sorted-map* []))

(defn- preservation-status-row
  "Insert the ORIGINAL operation's own conditional preservation status.
   This is descriptive source evidence, not an admitted proof."
  [status-map op-id result]
  (assoc status-map op-id (:preservation-status result)))

(defn- preservation-statuses
  "Account for every original operation in the invariant's status map.
   The caller separately validates compatible conditional induction."
  [results]
  (reduce-kv preservation-status-row (empty-preservation-statuses) results))

(defn- initial-status-row
  "Accumulate only an operation's original conditional INITIAL status."
  [statuses _operation-id result]
  (conj statuses (:initial-status result)))

(defn- initial-statuses
  "Extract distinct initial statuses from original operation-indexed results."
  [results]
  (reduce-kv initial-status-row #{} results))

(defn- induction-status-row
  "Accumulate only an operation's original conditional INDUCTION status."
  [statuses _operation-id result]
  (conj statuses (:induction-status result)))

(defn- induction-statuses
  "Extract distinct induction statuses from original operation results."
  [results]
  (reduce-kv induction-status-row #{} results))

(defn- preservation-step-row
  "Accumulate only an operation's original conditional PRESERVATION status."
  [statuses _operation-id result]
  (conj statuses (:preservation-status result)))

(defn- preservation-step-statuses
  "Extract distinct preservation statuses without losing operation evidence."
  [results]
  (reduce-kv preservation-step-row #{} results))

(defn- operation-invariant-row
  "Preserve the original operation ID and its selected invariant evidence.
   interfaces! has already validated complete operation/invariant coverage."
  [entries operation-id invariant-result]
  (assoc entries operation-id invariant-result))

(defn- operation-invariant-results
  "Retain all original operation/invariant evidence in deterministic order."
  [interfaces invariant-id]
  (reduce-kv (fn [entries operation-id operation]
               (operation-invariant-row
                entries operation-id
                (get-in operation [:conditional-invariant-results invariant-id])))
             (sorted-map* []) interfaces))

(defn- open-reason-row
  "Collect the original invariant's reason from each operation's evidence."
  [reasons _operation-id result]
  (conj reasons (:open-reason result)))

(defn- open-reasons
  "Keep distinct open-reason classifications across the original operations."
  [results]
  (reduce-kv open-reason-row #{} results))

(defn- consistent-invariant-statuses?
  "Check agreement across ORIGINAL per-operation conditional evidence.
   This is a consistency predicate over supplied status sets, not a proof
   that those sets came from authentic source or a valid induction theorem."
  [initial induction open-reasons steps]
  (let [all-checked? (and (= initial #{:checked-conditional-inference})
                          (= steps #{:checked-conditional-inference}))]
    (and (= 1 (count initial))
         (= 1 (count induction))
         (= 1 (count open-reasons))
         (set/subset? steps #{:checked-conditional-inference :open})
         (= (first induction)
            (if all-checked?
              :checked-conditional-induction :open)))))

(defn- invariant-results! [interfaces invariant-ids]
  (sorted-map*
   (for [invariant-id (sorted-ids invariant-ids)]
     (let [           results (operation-invariant-results interfaces invariant-id)
           values (vals results)
           initial (initial-statuses results)
           induction (induction-statuses results)
           open-reasons (open-reasons results)
           steps (preservation-step-statuses results)]
       (when-not (consistent-invariant-statuses?
                  initial induction open-reasons steps)
         (reject! :incompatible-invariant-evidence
                  {:invariant-id invariant-id}))
       [invariant-id
        (aggregate-invariant-entry
         (first initial)
         (preservation-statuses results)
         (first induction)
         (first open-reasons))]))))

(defn- interactions [interfaces]
  (sorted-map*
   (for [first-id (sorted-ids (keys interfaces))
         second-id (sorted-ids (keys interfaces))
         :when (not= first-id second-id)
         :let [first-op (get interfaces first-id)
               second-op (get interfaces second-id)
               written (:writes first-op)
               read-after-write (set/intersection written (:reads second-op))
               write-after-write (set/intersection written (:writes second-op))]]
     [[first-id second-id]
      {:first-writes-second-reads read-after-write
       :both-write write-after-write
       :potential-interference? (boolean (or (seq read-after-write)
                                             (seq write-after-write)))
       :commutativity :not-established
       :concurrency-refinement :not-established}])))

(defn- compose-derived! [derived]
  (let [interfaces (interfaces! derived)
        types (common-state-types! interfaces)
        invariant-results (invariant-results! interfaces (:invariant-ids derived))
        contracts
        (sorted-map*
         (for [[id operation] interfaces]
           [id (select-keys operation
                            [:operation-id :catalog-id :catalog-fingerprint
                             :source-fingerprint :subject-fingerprint
                             :transition-fingerprint :state-types :input-types
                             :guard :assignments :reads :writes :frame
                             :conditional-invariant-results
                             :original-open-obligation-ids
                             :open-meta-obligations
                             :contract-scope :guarantee-status
                             :physical-writer-coverage
                             :implementation-correspondence
                             :certificate :admission :authority])]))
        original (:original-open-obligation-ids-by-operation derived)
        open-meta (:open-meta-obligations derived)
        interference (interactions interfaces)
        identity (subject/describe-subject
                  {:kind :rule :id identity-id
                   :interface
                   {:version composition-version
                    :catalog-id (:catalog-id derived)
                    :catalog-fingerprint (:catalog-fingerprint derived)
                    :induction-fingerprint (:induction-fingerprint derived)
                    :interface-fingerprint (:fingerprint derived)
                    :operation-contracts contracts
                    :invariant-results invariant-results
                    :original-open-obligations original
                    :open-meta-obligations open-meta
                    :open-assumptions (:open-assumptions derived)
                    :interactions interference}})]
    {:type composition-type
     :version composition-version
     :fingerprint (:fingerprint identity)
     :catalog-id (:catalog-id derived)
     :catalog-fingerprint (:catalog-fingerprint derived)
     :induction-fingerprint (:induction-fingerprint derived)
     :interface-fingerprint (:fingerprint derived)
     :operation-ids (:operation-ids derived)
     :invariant-ids (:invariant-ids derived)
     :shared-state-types types
     :operation-contracts contracts
     :invariant-results invariant-results
     :interactions interference
     :original-open-obligation-ids-by-operation original
     :open-meta-obligations open-meta
     :open-assumptions (:open-assumptions derived)
     :scope :single-declared-abstract-machine
     :checking :source-derived-interface-compatibility
     :proof-status :conditional-inference-or-open
     :assumption-discharge :not-performed
     :physical-writer-coverage :not-established
     :implementation-correspondence :not-established
     :concurrency-refinement :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn compose-operation-interfaces!
  "Revalidate the live v1440 closure and independently derive *all* v1442
   interfaces from ORIGINAL source. Compose only if a single exact state
   schema and complete per-invariant evidence are shared by every operation.
   Returns a copyable, untrusted conditional report, never a proof token."
  [catalog current-graph closure]
  (compose-derived!
   (iface/derive-operation-interfaces! catalog current-graph closure)))

(defn require-current-operation-composition!
  "Compare an untrusted report against a fresh source-derived composition.
   A matching copy still confers no proof, admission, or runtime authority."
  [prior catalog current-graph closure]
  (let [now (compose-operation-interfaces! catalog current-graph closure)]
    (when-not (and (map? prior) (not (record? prior))
                   (nil? (meta prior)) (= prior now))
      (reject! :stale-or-substituted-operation-composition
               {:expected (:fingerprint now) :actual (:fingerprint prior)}))
    now))
