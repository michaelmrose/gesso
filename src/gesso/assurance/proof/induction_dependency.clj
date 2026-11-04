(ns gesso.assurance.proof.induction-dependency
  "Exact SOURCE-RELATIVE dependency closure for checked conditional induction
   (v1282). Derive every proof dependency from the ORIGINAL operation catalog;
   never accept a caller's subset as the definition of proof completeness.

   The graph contains the induction-certificate subject (sole root), one
   catalog identity, the shared initial condition, each declared invariant,
   and each declared operation together with its source-derived evidence.
   Its edges encode their dependencies. All source declarations and the
   complete inductive argument are rechecked before a graph is accepted.

   This is stricter than the generic structural dependency DAG: missing or
   additional nodes, substituted subjects, changed edges or stale source
   identities fail even when they could form a valid generic DAG.

   check-induction-dependencies! issues an unforgeable *local graph-closure*
   handle. It does NOT issue a proof certificate, discharge opaque premises,
   establish real initializer/writer coverage, prove checker soundness, or
   authorize runtime effects. Certificate admission in v1280 does not yet
   require this handle; future trusted consumers must explicitly integrate
   it rather than silently treating generic DAG acceptance as full closure.

   The in-process security model excludes malicious reflection, Var mutation
   with equivalent JVM privileges, or modified classes, as in v1280.
   This module is JVM-only because it uses the existing JVM dependency and
   admission registries."
  (:require [clojure.set :as set]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.subject :as subject])
  (:import (java.util WeakHashMap)))

(def dependency-version 1)
(def dependency-type
  :gesso.assurance.proof.induction-dependency/in-process-source-closure)
(def ^:private identity-id
  :gesso.assurance.proof.induction-dependency/source-closure-v1)
(def ^:private node-namespace "gesso.assurance.proof.induction-dependency")
(def ^:private open-boundaries
  #{:induction-rule-soundness
    :initial-state-existence
    :external-initializer-coverage
    :external-operation-set-completeness
    :physical-write-path-coverage
    :implementation-correspondence
    :checker-meta-soundness})

(deftype InductionDependencyHandle []
  Object
  (toString [_] "#<gesso.assurance/induction-source-dependency-closure>"))

(def ^:private ^WeakHashMap live-handles (WeakHashMap.))

(defn- fail! [reason at details]
  (throw (ex-info "Invalid Gesso induction proof-source dependency closure."
                  (merge {:error/type ::invalid-induction-dependencies
                          :reason reason :at at} details))))

(defn- source-id [role original-id]
  ;; A reversible encoding, avoiding collisions between IDs such as
  ;; :a.b/c and :a/b.c, and separating roles even if raw IDs coincide.
  (keyword node-namespace
           (str (name role) "-"
                (apply str (map #(format "%04x" (int %))
                                (str original-id))))))

(defn- raw-node [id interface]
  {:kind :rule :id id :interface interface})

(defn- fingerprint [raw]
  (:fingerprint (subject/describe-subject raw)))

(defn- edge [raw]
  {:id (:id raw) :fingerprint (fingerprint raw)})

(defn- make-node [raw dependencies]
  {:id (:id raw)
   :subject raw
   :depends-on (mapv edge (sort-by (comp str :id) dependencies))})

(defn- semantic-graph!
  "Derive all required graph nodes and edges without creating authority.
   Upstream source rechecking is mandatory; copied induction reports cannot
   be passed here in place of the original catalog."
  [catalog]
  (let [root (certificate/induction-certificate-subject! catalog)
        report (induction/check-induction! catalog)
        operations (:declared-operation-ids report)
        invariants (:declared-invariant-ids report)
        entry-by-id (into {} (map (juxt :id identity)) (:operations catalog))
        evidence-by-id (into {} (map (juxt :operation-id identity))
                             (:operation-evidence report))
        state-types (get-in (entry-by-id (first (sort-by str operations)))
                            [:construction :state-types])
        catalog-node (raw-node (source-id :catalog (:catalog-id report))
                               {:role :operation-catalog
                                :catalog-id (:catalog-id report)
                                :catalog-fingerprint (:catalog-fingerprint report)
                                :operation-ids operations
                                :invariant-ids invariants})
        initial-node (raw-node (source-id :initial (:catalog-id report))
                               {:role :shared-initial-condition
                                :catalog-id (:catalog-id report)
                                :state-types state-types
                                :source (:initial-condition catalog)})
        invariant-nodes
        (mapv (fn [id]
                (raw-node (source-id :invariant id)
                          {:role :declared-invariant
                           :invariant-id id
                           :state-types state-types
                           :source (get-in catalog [:invariants id])}))
              (sort-by str invariants))
        operation-nodes
        (mapv (fn [id]
                (raw-node (source-id :operation id)
                          {:role :declared-abstract-operation
                           :operation-id id
                           :construction (:construction (entry-by-id id))
                           :checked-evidence (evidence-by-id id)}))
              (sort-by str operations))
        leaves (into [initial-node] invariant-nodes)
        graph {:roots #{(:id root)}
               :nodes (vec (concat [(make-node root [catalog-node])
                                    (make-node catalog-node
                                               (into leaves operation-nodes))
                                    (make-node initial-node [])]
                                   (map #(make-node % []) invariant-nodes)
                                   (map #(make-node % leaves) operation-nodes)))}]
    (when-not (and (= operations (set (keys entry-by-id)))
                   (= operations (set (keys evidence-by-id)))
                   (= invariants (set (keys (:invariants catalog))))
                   (= (count (:nodes graph))
                      (+ 3 (count operations) (count invariants)))
                   (= (count (:nodes graph))
                      (count (set (map :id (:nodes graph))))))
      (fail! :derived-coverage-conflict [:catalog] {}))
    {:graph graph
     :root root
     :report report
     :source-fingerprint
     (fingerprint (raw-node identity-id
                            {:catalog catalog
                             :induction-fingerprint (:fingerprint report)}))}))

(defn- graph-identity [graph]
  {:roots (:roots graph)
   :nodes (into {}
                (map (fn [{:keys [id subject depends-on]}]
                       [id {:fingerprint (fingerprint subject)
                            :subject subject
                            :depends-on (set depends-on)}]))
                (:nodes graph))})

(defn- expected-graph! [catalog]
  (:graph (semantic-graph! catalog)))

(defn build-induction-dependency-graph!
  "Construct the exact source-derived DAG with fresh structural node handles.
   The returned graph is input to check-induction-dependencies!, NOT itself
   evidence of checking or an admitted certificate."
  [catalog]
  (let [graph (expected-graph! catalog)]
    (update graph :nodes
            (fn [nodes]
              (mapv #(assoc % :subject-handle
                            (admission/admit-subject! (:subject %)))
                    nodes)))))

(defn- exact-graph! [expected supplied]
  (when-not (and (map? supplied) (not (record? supplied))
                 (nil? (meta supplied))
                 (= #{:roots :nodes} (set (keys supplied)))
                 (set? (:roots supplied))
                 (nil? (meta (:roots supplied)))
                 (vector? (:nodes supplied))
                 (nil? (meta (:nodes supplied))))
    (fail! :invalid-graph-shape [:graph] {}))
  (doseq [[index node] (map-indexed vector (:nodes supplied))]
    (when-not (and (map? node) (not (record? node))
                   (nil? (meta node))
                   (= #{:id :subject :subject-handle :depends-on}
                      (set (keys node)))
                   (vector? (:depends-on node))
                   (nil? (meta (:depends-on node)))
                   (every? (fn [edge]
                             (and (map? edge) (not (record? edge))
                                  (nil? (meta edge))
                                  (= #{:id :fingerprint} (set (keys edge)))))
                           (:depends-on node)))
      (fail! :invalid-node-shape [:graph :nodes index] {})))
  (let [expected-ids (set (map :id (:nodes expected)))
        actual-ids (mapv :id (:nodes supplied))
        supplied-ids (set actual-ids)]
    (when-not (and (= expected-ids supplied-ids)
                   (= (count expected-ids) (count actual-ids))
                   (= (:roots expected) (:roots supplied)))
      (fail! :source-dependency-coverage-mismatch [:graph]
             {:missing (set/difference expected-ids supplied-ids)
              :unexpected (set/difference supplied-ids expected-ids)})))
  (when-not (= (graph-identity expected) (graph-identity supplied))
    (fail! :source-dependency-content-mismatch [:graph] {}))
  ;; Authenticate the live structural handles, exact edge fingerprints,
  ;; acyclicity, reachability and single-root property via the generic checker.
  (dependency/check-graph! supplied))

(defn- registered [handle]
  (when (instance? InductionDependencyHandle handle)
    (locking live-handles (.get live-handles handle))))

(defn checked-induction-dependencies?
  "True only for an in-process source-closure handle, not proof authority."
  [handle]
  (boolean (registered handle)))

(defn check-induction-dependencies!
  "Recheck the ORIGINAL catalog and its full conditional induction, then
   require the supplied live DAG to match ALL source-derived dependency nodes
   and edges, not merely a chosen subset. Return an opaque local closure handle.

   Caller-supplied proof reports, omitted dependencies, additional nodes,
   forged success fields and stale identities cannot alter this source scope.
   This does NOT establish production operation/initializer completeness."
  [catalog current-graph]
  (let [{:keys [graph report source-fingerprint]} (semantic-graph! catalog)
        graph-handle (exact-graph! graph current-graph)
        snap (graph-identity graph)
        identity (fingerprint (raw-node identity-id
                                        {:source-fingerprint source-fingerprint
                                         :graph snap}))
        entry {:graph-handle graph-handle
               :graph-snapshot snap
               :source-fingerprint source-fingerprint
               :induction-fingerprint (:fingerprint report)
               :catalog-fingerprint (:catalog-fingerprint report)
               :catalog-id (:catalog-id report)
               :operation-ids (:declared-operation-ids report)
               :invariant-ids (:declared-invariant-ids report)
               :open-meta-obligations (:open-meta-obligations report)
               :original-open-obligation-ids-by-operation
               (:original-open-obligation-ids-by-operation report)
               :fingerprint identity
               :node-count (count (:nodes graph))
               :edge-count (reduce + (map #(count (:depends-on %))
                                         (:nodes graph)))}
        handle (InductionDependencyHandle.)]
    (locking live-handles (.put live-handles handle entry))
    handle))

(defn describe-induction-dependencies
  "Return COPYABLE descriptive evidence; it cannot recreate the live handle."
  [handle]
  (let [entry (registered handle)]
    (when-not entry
      (fail! :unrecognized-dependency-handle [:handle] {}))
    (-> entry
        (dissoc :graph-handle :graph-snapshot)
        (assoc :type dependency-type
               :version dependency-version
               :checking :original-proof-source-and-exact-dependency-graph
               :proof-status :source-dependencies-structurally-checked
               :open-external-obligations open-boundaries
               :assumption-discharge :not-performed
               :certificate :not-issued
               :implementation-correspondence :not-established
               :runtime-enforcement :not-established
               :authority :none))))

(defn require-current-induction-dependencies!
  "Require a live source-closure handle to remain current against ORIGINAL
   current catalog source and a FRESH current dependency graph declaration.
   Never use a copied report or a stale cached graph in place of either."
  [handle current-catalog current-graph]
  (let [prior (registered handle)]
    (when-not prior
      (fail! :unrecognized-dependency-handle [:handle] {}))
    (let [{:keys [graph report source-fingerprint]}
          (semantic-graph! current-catalog)
          now (graph-identity graph)]
      (when-not (and (= (:graph-snapshot prior) now)
                     (= (:source-fingerprint prior) source-fingerprint)
                     (= (:induction-fingerprint prior) (:fingerprint report))
                     (= (:catalog-fingerprint prior)
                        (:catalog-fingerprint report)))
        (fail! :stale-source-dependency-closure [:catalog] {}))
      (when-not (= now (graph-identity current-graph))
        (fail! :source-dependency-content-mismatch [:graph] {}))
      (dependency/require-current-graph! (:graph-handle prior) current-graph))
    handle))
