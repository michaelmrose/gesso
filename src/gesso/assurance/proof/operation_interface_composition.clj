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

(defn- invariant-results! [interfaces invariant-ids]
  (sorted-map*
   (for [invariant-id (sorted-ids invariant-ids)]
     (let [results (sorted-map*
                    (for [[op-id op] interfaces]
                      [op-id (get-in op [:conditional-invariant-results
                                         invariant-id])]))
           values (vals results)
           initial (set (map :initial-status values))
           induction (set (map :induction-status values))
           open-reasons (set (map :open-reason values))
           steps (into #{} (map :preservation-status) values)
           all-checked? (and (= initial #{:checked-conditional-inference})
                             (= steps #{:checked-conditional-inference}))]
       (when-not (and (= 1 (count initial))
                      (= 1 (count induction))
                      (= 1 (count open-reasons))
                      (set/subset? steps #{:checked-conditional-inference :open})
                      (= (first induction)
                         (if all-checked?
                           :checked-conditional-induction :open)))
         (reject! :incompatible-invariant-evidence
                  {:invariant-id invariant-id}))
       [invariant-id
        {:initial-status (first initial)
         :preservation-by-operation
         (sorted-map*
          (for [[op-id result] results]
            [op-id (:preservation-status result)]))
         :induction-status (first induction)
         :open-reason (first open-reasons)}]))))

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
