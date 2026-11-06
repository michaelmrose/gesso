(ns gesso.assurance.proof.application-dependency
  "Source-derived application ASSURANCE dependency closure (v1440).

   This extends, rather than replaces, the exact induction source graph:
   the original catalog/initializer/invariants/operations/conditional proof
   root remain intact. Additional nodes expose the source-recomputed induction
   ledger, each operation's evidence and original obligations, each invariant's
   base/step result, and every open meta premise. The sole new root depends on
   the complete original induction argument AND the ledger, so a valid subset
   of a generic DAG cannot silently become full declared-source closure.

   This is CLOSED ONLY FOR THE DECLARED ABSTRACT CATALOG. A live handle is an
   in-process structural/source check, not a proof, application admission,
   physical writer inventory, XTDB refinement, certificate, or authority. A
   caller-selected catalog cannot authenticate an installed application.
   require-current-application-dependencies! requires fresh trusted source AND
   a fresh graph; copied reports and forged graph handles cannot replace them.

   The existing JVM-local structural admission threat model applies: arbitrary
   reflection, compiled-code alteration, or equivalent trusted-JVM privileges
   are outside the handled adversary model."
  (:require [clojure.set :as set]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.induction-dependency :as induction-dependency]
            [gesso.assurance.proof.induction-ledger :as ledger]
            [gesso.assurance.subject :as subject])
  (:import (java.util WeakHashMap)))

(def closure-version 1)
(def closure-type
  :gesso.assurance.proof.application-dependency/in-process-source-closure)
(def ^:private node-namespace "gesso.assurance.proof.application-dependency")

(deftype ApplicationDependencyHandle []
  Object
  (toString [_] "#<gesso.assurance/application-source-dependency-closure>"))

(def ^:private ^WeakHashMap live-handles (WeakHashMap.))

(defn- reject! [reason data]
  (throw (ex-info "Invalid Gesso application-assurance source dependency closure."
                  (merge {:error/type ::invalid-application-dependencies
                          :reason reason} data))))

(defn- role-id [role original]
  ;; Collision-free across :a.b/c and :a/b.c, and disjoint from existing
  ;; induction dependency IDs by namespace and role.
  (keyword node-namespace
           (str (name role) "-"
                (apply str (map #(format "%04x" (int %)) (str original))))))

(defn- raw-node [id interface]
  {:kind :rule :id id :interface interface})

(defn- fingerprint [raw]
  (:fingerprint (subject/describe-subject raw)))

(defn- edge [raw]
  {:id (:id raw) :fingerprint (fingerprint raw)})

(defn- node [raw sources]
  {:id (:id raw)
   :subject raw
   :subject-handle (admission/admit-subject! raw)
   :depends-on (mapv edge (sort-by (comp str :id) sources))})

(defn- source-node! [base role id]
  (let [matches (filter (fn [n]
                          (and (= role (get-in n [:subject :interface :role]))
                               (= id (case role
                                       :declared-invariant
                                       (get-in n [:subject :interface :invariant-id])
                                       :declared-abstract-operation
                                       (get-in n [:subject :interface :operation-id])
                                       :shared-initial-condition
                                       (get-in n [:subject :interface :catalog-id])
                                       :operation-catalog
                                       (get-in n [:subject :interface :catalog-id])
                                       nil))))
                        (:nodes base))]
    (when-not (= 1 (count matches))
      (reject! :original-source-node-coverage-mismatch
               {:role role :id id :count (count matches)}))
    (:subject (first matches))))

(defn- source-graph! [catalog]
  ;; Both siblings reconstruct their results from the ORIGINAL source. No
  ;; caller-supplied proof or ledger reports are accepted as inputs.
  (let [base (induction-dependency/build-induction-dependency-graph! catalog)
        report (ledger/check-induction-ledger! catalog)
        catalog-id (:catalog-id report)
        ops (sort-by str (:declared-operation-ids report))
        invs (sort-by str (:declared-invariant-ids report))
        checked-steps (:checked-abstract-steps report)
        open-steps (:open-abstract-steps report)
        steps (into {} (map (juxt :invariant-id identity))
                    (concat checked-steps open-steps))
        originals (:original-open-obligation-ids-by-operation report)
        original-ops (into {} (map (fn [id]
                                    [id (source-node! base
                                                      :declared-abstract-operation
                                                      id)])) ops)
        original-invs (into {} (map (fn [id]
                                     [id (source-node! base
                                                       :declared-invariant
                                                       id)])) invs)
        original-initial (source-node! base :shared-initial-condition catalog-id)
        original-root (:subject (first (filter #(contains? (:roots base) (:id %))
                                               (:nodes base))))
        open-node (raw-node (role-id :assumptions catalog-id)
                            {:role :original-open-assumptions
                             :catalog-id catalog-id
                             :ledger-fingerprint (:fingerprint report)
                             :open-meta-obligations (:open-meta-obligations report)
                             :open-assumptions (:open-assumptions report)
                             :original-open-obligations originals})
        operation-evidence
        (mapv (fn [id]
                (raw-node (role-id :operation-evidence id)
                          {:role :conditional-operation-evidence
                           :operation-id id
                           :source-catalog-fingerprint
                           (:catalog-fingerprint report)
                           :original-open-obligation-ids (get originals id)})) ops)
        invariant-evidence
        (mapv (fn [id]
                (raw-node (role-id :invariant-evidence id)
                          {:role :conditional-invariant-evidence
                           :invariant-id id
                           :source-catalog-fingerprint
                           (:catalog-fingerprint report)
                           :checked-step (get steps id)})) invs)
        ledger-node (raw-node (role-id :ledger catalog-id)
                              {:role :source-derived-induction-ledger
                               :catalog-id catalog-id
                               :catalog-fingerprint (:catalog-fingerprint report)
                               :induction-fingerprint (:induction-fingerprint report)
                               :ledger-fingerprint (:fingerprint report)
                               :open-meta-obligations (:open-meta-obligations report)})
        root (raw-node (role-id :application catalog-id)
                       {:role :declared-application-assurance
                        :catalog-id catalog-id
                        :catalog-fingerprint (:catalog-fingerprint report)
                        :induction-fingerprint (:induction-fingerprint report)
                        :ledger-fingerprint (:fingerprint report)
                        :scope :closed-declared-abstract-machine-only})
        extras (vec (concat
                     [(node open-node (into [original-initial] (vals original-ops)))]
                     (map (fn [id raw]
                            (node raw (into [(get original-ops id) open-node]
                                            (vals original-invs))))
                          ops operation-evidence)
                     (map (fn [id raw]
                            (node raw (concat [(get original-invs id)
                                               original-initial]
                                              operation-evidence)))
                          invs invariant-evidence)
                     [(node ledger-node (concat [original-root open-node]
                                                operation-evidence
                                                invariant-evidence))
                      (node root [original-root ledger-node])]))
        graph {:roots #{(:id root)}
               :nodes (into (:nodes base) extras)}]
    (when-not (and (= (set ops) (set (keys originals)))
                   (= (set invs) (set (keys steps)))
                   (= 1 (count (:roots base)))
                   (= (+ (count (:nodes base)) 3 (count ops) (count invs))
                      (count (:nodes graph)))
                   (= (count (:nodes graph)) (count (set (map :id (:nodes graph))))))
      (reject! :source-evidence-coverage-mismatch {}))
    {:base base :graph graph :ledger report
     :catalog-id catalog-id
     :operation-ids (set ops) :invariant-ids (set invs)}))

(defn- shape! [graph]
  (when-not (and (map? graph) (not (record? graph))
                 (nil? (meta graph))
                 (= #{:roots :nodes} (set (keys graph)))
                 (set? (:roots graph))
                 (nil? (meta (:roots graph)))
                 (vector? (:nodes graph))
                 (nil? (meta (:nodes graph))))
    (reject! :invalid-graph-shape {}))
  (doseq [n (:nodes graph)]
    (when-not (and (map? n) (not (record? n))
                   (nil? (meta n))
                   (= #{:id :subject :subject-handle :depends-on}
                      (set (keys n)))
                   (vector? (:depends-on n))
                   (nil? (meta (:depends-on n))))
      (reject! :invalid-node-shape {:id (:id n)})))
  graph)

(defn- content [graph]
  (let [g (shape! graph)
        nodes (:nodes g)
        ids (mapv :id nodes)]
    (when-not (= (count ids) (count (set ids)))
      (reject! :duplicate-node {}))
    {:roots (:roots g)
     :nodes (into {} (map (fn [n]
                            [(:id n) {:subject (:subject n)
                                      :fingerprint (fingerprint (:subject n))
                                      :depends-on (set (:depends-on n))}]))
                  nodes)}))

(defn- exact! [expected candidate]
  (let [before (content expected)
        after (content candidate)]
    (when-not (= (:roots before) (:roots after))
      (reject! :root-scope-mismatch {}))
    (let [a (set (keys (:nodes before)))
          b (set (keys (:nodes after)))]
      (when-not (= a b)
        (reject! :source-dependency-coverage-mismatch
                 {:missing (set/difference a b)
                  :unexpected (set/difference b a)})))
    (when-not (= before after)
      (reject! :source-dependency-content-mismatch {})))
  candidate)

(defn- inner-graph [base candidate]
  (let [ids (set (map :id (:nodes base)))]
    {:roots (:roots base)
     :nodes (vec (filter #(contains? ids (:id %)) (:nodes candidate)))}))

(defn build-application-dependency-graph!
  "Generate the whole DECLARED abstract application assurance DAG, including
   original source, conditional evidence, and undischarged premises. The raw
   graph is not a proof or an installed-application inventory."
  [catalog]
  (:graph (source-graph! catalog)))

(defn check-application-dependencies!
  "Recompute all nodes from ORIGINAL catalog source, insist on exact graph
   coverage, and check both the existing induction closure and the complete
   structural graph. Returns only an opaque JVM-local closure handle."
  [catalog current-graph]
  (let [{:keys [graph base ledger operation-ids invariant-ids catalog-id]}
        (source-graph! catalog)
        _ (exact! graph current-graph)
        inner (inner-graph base current-graph)
        inner-handle (induction-dependency/check-induction-dependencies!
                      catalog inner)
        graph-handle (dependency/check-graph! current-graph)
        entry {:graph-handle graph-handle :inner-handle inner-handle
               :graph-content (content graph) :ledger ledger
               :catalog-id catalog-id :operation-ids operation-ids
               :invariant-ids invariant-ids
               :node-count (count (:nodes graph))
               :edge-count (reduce + (map #(count (:depends-on %))
                                          (:nodes graph)))}
        handle (ApplicationDependencyHandle.)]
    (locking live-handles (.put live-handles handle entry))
    handle))

(defn- entry! [handle]
  (let [entry (when (instance? ApplicationDependencyHandle handle)
                (locking live-handles (.get live-handles handle)))]
    (when-not entry
      (reject! :unrecognized-closure-handle {}))
    entry))

(defn checked-application-dependencies?
  "Recognize a locally checked source graph, NOT an application certificate."
  [handle]
  (boolean (and (instance? ApplicationDependencyHandle handle)
                (locking live-handles (.get live-handles handle)))))

(defn describe-application-dependencies
  "Copyable non-authoritative summary retaining EVERY open original premise."
  [handle]
  (let [{:keys [ledger catalog-id operation-ids invariant-ids
                node-count edge-count]} (entry! handle)]
    {:type closure-type :version closure-version
     :catalog-id catalog-id
     :catalog-fingerprint (:catalog-fingerprint ledger)
     :induction-fingerprint (:induction-fingerprint ledger)
     :ledger-fingerprint (:fingerprint ledger)
     :operation-ids operation-ids :invariant-ids invariant-ids
     :node-count node-count :edge-count edge-count
     :open-meta-obligations (:open-meta-obligations ledger)
     :open-assumptions (:open-assumptions ledger)
     :original-open-obligation-ids-by-operation
     (:original-open-obligation-ids-by-operation ledger)
     :checking :original-source-and-exact-application-dependency-graph
     :scope :closed-declared-abstract-machine-only
     :proof-status :structural-source-dependency-closure-only
     :assumption-discharge :not-performed
     :installed-writer-coverage :not-established
     :implementation-correspondence :not-established
     :certificate :not-issued :admission :not-admitted
     :authority :none}))

(defn require-current-application-dependencies!
  "Require the live handle, fresh trusted ORIGINAL catalog, and a fresh graph.
   Source differences and stale, missing or rewritten evidence fail closed."
  [handle current-catalog current-graph]
  (let [{:keys [graph-handle inner-handle graph-content]} (entry! handle)
        {:keys [base graph]} (source-graph! current-catalog)]
    (when-not (= graph-content (content graph))
      (reject! :stale-application-source {}))
    (exact! graph current-graph)
    (induction-dependency/require-current-induction-dependencies!
     inner-handle current-catalog (inner-graph base current-graph))
    (dependency/require-current-graph! graph-handle current-graph)
    handle))
