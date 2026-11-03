(ns gesso.assurance.dependency
  "Exact, fail-closed structural dependency graphs for Gesso assurance (v1217).

   Every node binds a qualified node identity to a RAW declarative semantic
   subject and a live v1215 SUBJECT-IDENTITY admission handle. Every edge
   carries the exact expected *subject fingerprint* of its dependency; a
   matching name without a matching interface is NOT sufficient.

   check-graph! verifies nodes, identity-bound handles, edges, roots, and DAG
   structure before issuing a NON-TRANSFERABLE in-process checked-graph handle.
   require-current-graph! revalidates a fresh declaration and rejects changes
   to any node or dependency, including transitive descendants of changes.

   This is STRUCTURAL CHECKING ONLY. A checked graph is NOT a proof, does NOT
   discharge any assumption, does NOT validate application-installed handlers,
   and has no runtime authorization or production correspondence authority.
   It is a snapshot: any consumer asserting freshness must supply the current
   declaration sourced from its own trusted assembly, NOT caller-selected EDN.

   As with v1215, arbitrary trusted-JVM reflection or Var replacement is not
   excluded. Weak-key registration prevents copied EDN and manually created
   handle instances from acting as checked graphs."
  (:require [clojure.set :as set]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.subject :as subject])
  (:import (java.util WeakHashMap)))

(def graph-version 1)
(def graph-type :gesso.assurance.dependency/structurally-checked-graph)
(def ^:private max-nodes 512)
(def ^:private max-edges 4096)
(def ^:private exact-graph-keys #{:nodes :roots})
(def ^:private exact-node-keys #{:id :subject :subject-handle :depends-on})
(def ^:private exact-edge-keys #{:id :fingerprint})

(deftype CheckedGraph []
  Object
  (toString [_] "#<gesso.assurance/checked-dependency-graph>"))

(def ^:private ^WeakHashMap live-graphs (WeakHashMap.))

(defn- graph-error!
  [reason data]
  (throw (ex-info "Invalid Gesso assurance dependency graph."
                  (merge {:error/type ::invalid-dependency-graph
                          :reason reason}
                         data))))

(defn- qualified-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- exact-keys!
  [value expected at]
  (when-not (and (map? value) (not (record? value)))
    (graph-error! :expected-map {:at at}))
  (let [actual (set (keys value))]
    (when-not (= expected actual)
      (graph-error! :wrong-keys
                    {:at at
                     :missing (set/difference expected actual)
                     :unexpected (set/difference actual expected)})))
  value)

(defn- fingerprint? [x]
  (and (string? x) (boolean (re-matches #"sha256:[0-9a-f]{64}" x))))

(defn- validate-node!
  [node]
  (exact-keys! node exact-node-keys :node)
  (let [id (:id node)
        raw-subject (:subject node)]
    (when-not (qualified-keyword? id)
      (graph-error! :invalid-node-id {:id id}))
    ;; Never trust a caller-provided :fingerprint or :valid? on the raw subject.
    ;; The subject and admission checks validate this object independently.
    (let [identity (subject/describe-subject raw-subject)]
      (when-not (= id (get-in identity [:subject :id]))
        (graph-error! :node-subject-id-mismatch
                      {:id id :subject-id (get-in identity [:subject :id])}))
      (admission/require-admitted-subject! (:subject-handle node) raw-subject)
      (when-not (vector? (:depends-on node))
        (graph-error! :invalid-dependencies {:id id}))
      (let [edges
            (mapv (fn [edge]
                    (exact-keys! edge exact-edge-keys [:node id :edge])
                    (when-not (qualified-keyword? (:id edge))
                      (graph-error! :invalid-dependency-id
                                    {:node id :dependency (:id edge)}))
                    (when-not (fingerprint? (:fingerprint edge))
                      (graph-error! :invalid-dependency-fingerprint
                                    {:node id :dependency (:id edge)}))
                    edge)
                  (:depends-on node))
            ids (map :id edges)]
        (when-not (= (count ids) (count (distinct ids)))
          (graph-error! :duplicate-dependency {:node id}))
        (when (some #(= id %) ids)
          (graph-error! :self-dependency {:node id}))
        {:id id
         :fingerprint (:fingerprint identity)
         :depends-on (vec (sort-by (comp str :id) edges))}))))

(defn- dependency-order!
  "Return dependency-first order, rejecting every directed cycle."
  [nodes]
  (let [visiting (atom #{})
        visited (atom #{})
        order (transient [])]
    (letfn [(visit! [id]
              (when (contains? @visiting id)
                (graph-error! :cycle {:node id}))
              (when-not (contains? @visited id)
                (swap! visiting conj id)
                (doseq [{target :id} (:depends-on (get nodes id))]
                  (visit! target))
                (swap! visiting disj id)
                (swap! visited conj id)
                (conj! order id)))]
      (doseq [id (sort-by str (keys nodes))]
        (visit! id))
      (persistent! order))))

(defn- build-snapshot!
  [raw]
  (exact-keys! raw exact-graph-keys :graph)
  (when-not (and (vector? (:nodes raw)) (seq (:nodes raw)))
    (graph-error! :invalid-nodes {}))
  (when (> (count (:nodes raw)) max-nodes)
    (graph-error! :node-limit {:maximum max-nodes}))
  (when-not (and (set? (:roots raw)) (seq (:roots raw))
                 (every? qualified-keyword? (:roots raw)))
    (graph-error! :invalid-roots {}))
  (let [nodes (mapv validate-node! (:nodes raw))
        ids (map :id nodes)]
    (when-not (= (count ids) (count (distinct ids)))
      (graph-error! :duplicate-node {}))
    (let [indexed (into {} (map (juxt :id identity) nodes))
          root-set (:roots raw)
          edges (reduce + 0 (map #(count (:depends-on %)) nodes))]
      (when (> edges max-edges)
        (graph-error! :edge-limit {:maximum max-edges}))
      (doseq [root root-set]
        (when-not (contains? indexed root)
          (graph-error! :missing-root {:root root})))
      (doseq [node nodes, edge (:depends-on node)]
        (let [target (get indexed (:id edge))]
          (when-not target
            (graph-error! :missing-dependency
                          {:node (:id node) :dependency (:id edge)}))
          (when-not (= (:fingerprint edge) (:fingerprint target))
            (graph-error! :stale-dependency
                          {:node (:id node)
                           :dependency (:id edge)
                           :expected (:fingerprint edge)
                           :actual (:fingerprint target)}))))
      (let [order (dependency-order! indexed)
            ;; An unreferenced node must not be smuggled into the graph's
            ;; purported certification scope. Every node must feed a root.
            reachable (loop [todo (seq root-set) seen #{}]
                        (if-let [id (first todo)]
                          (if (contains? seen id)
                            (recur (next todo) seen)
                            (recur (concat (map :id (:depends-on (get indexed id)))
                                           (next todo))
                                   (conj seen id)))
                          seen))]
        (when-not (= reachable (set (keys indexed)))
          (graph-error! :unreachable-node
                        {:nodes (set/difference (set (keys indexed)) reachable)}))
        {:nodes indexed :roots root-set :dependency-order order}))))

(defn- live-entry!
  [handle]
  (let [entry (when (instance? CheckedGraph handle)
                (locking live-graphs (.get live-graphs handle)))]
    (when-not entry
      (graph-error! :unrecognized-graph {}))
    entry))

(defn checked-graph?
  "True only for an in-process graph handle issued by check-graph!.
   This is STRUCTURAL acceptance, not theorem or application certification."
  [value]
  (boolean (and (instance? CheckedGraph value)
                (locking live-graphs (.get live-graphs value)))))

(defn check-graph!
  "Structurally check and register a *snapshot* of a dependency DAG.

   Input is EXACTLY:
   {:nodes [{:id :example/operation
             :subject <raw semantic subject, same :id>
             :subject-handle <v1215 live subject handle>
             :depends-on [{:id :example/policy
                           :fingerprint <policy subject sha256 fingerprint>}]}]
    :roots #{:example/operation}}

   This API accepts caller-provided subjects only as declarations. Callers who
   need applicability to a real assembly MUST obtain this graph from its actual
   trusted assembly, then use require-current-graph! against fresh assembly
   declarations. Neither action proves installed handler correspondence."
  [raw]
  (let [snapshot (build-snapshot! raw)
        handle (CheckedGraph.)]
    (locking live-graphs (.put live-graphs handle snapshot))
    handle))

(defn describe-graph
  "COPYABLE non-authoritative summary. Never treat this map as a handle."
  [handle]
  (let [snapshot (live-entry! handle)]
    {:type graph-type
     :version graph-version
     :nodes (:nodes snapshot)
     :roots (:roots snapshot)
     :dependency-order (:dependency-order snapshot)
     :checking :structural-only
     :proof-status :not-proved
     :assumption-discharge :not-established
     :implementation-correspondence :not-established
     :admission :not-a-proof
     :authority :none}))

(defn require-current-graph!
  "Authenticate handle AND reconstruct the graph from CURRENT trusted input.

   Rejects any changed node, reference, root or addition/removal. Current input
   must come from the consuming verifier's actual assembly, not an old captured
   snapshot supplied by an untrusted caller. Returns the ORIGINAL handle only."
  [handle current]
  (let [prior (live-entry! handle)
        now (build-snapshot! current)]
    (when-not (= prior now)
      (graph-error! :stale-graph
                    {:prior-nodes (set (keys (:nodes prior)))
                     :current-nodes (set (keys (:nodes now)))}))
    handle))

(defn affected-nodes
  "Return the transitive reverse-dependency closure for changed node IDs.

   Includes the changed nodes themselves. This is an impact calculation on the
   registered snapshot, NOT a claim of freshness, proof invalidation in a
   deployed application, or automatic certificate revocation. A dependent
   certificate must be re-checked against current subject identities."
  [handle changed-ids]
  (let [snapshot (live-entry! handle)
        indexed (:nodes snapshot)]
    (when-not (and (set? changed-ids) (seq changed-ids)
                   (every? qualified-keyword? changed-ids))
      (graph-error! :invalid-changed-nodes {}))
    (doseq [id changed-ids]
      (when-not (contains? indexed id)
        (graph-error! :unknown-changed-node {:node id})))
    (let [reverse-edges
          (reduce (fn [acc [source {:keys [depends-on]}]]
                    (reduce (fn [m {:keys [id]}]
                              (update m id (fnil conj #{}) source))
                            acc depends-on))
                  {} indexed)]
      (loop [todo (seq changed-ids) impacted #{}]
        (if-let [id (first todo)]
          (if (contains? impacted id)
            (recur (next todo) impacted)
            (recur (concat (get reverse-edges id) (next todo))
                   (conj impacted id)))
          impacted)))))
