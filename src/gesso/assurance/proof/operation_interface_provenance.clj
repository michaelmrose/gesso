(ns gesso.assurance.proof.operation-interface-provenance
  "v1444: inspectable source and dependency provenance for a v1443 composition.

   Recheck the composition against the ORIGINAL catalog and locally authenticated
   v1440 dependency closure before emitting a deterministic, source-relative
   evidence manifest. Include every checked graph node and edge, source
   fingerprints for each operation and invariant, the induction/operation/
   composition identity chain, and every still-open assumption.

   This is an UNTRUSTED, copyable, structural diagnostic artifact: neither its
   digest nor a successful recheck issues a proof, discharges assumptions,
   establishes physical writer coverage, or authorizes production execution."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.proof.operation-interface :as iface]
            [gesso.assurance.proof.operation-interface-composition :as comp]
            [gesso.assurance.subject :as subject]))

(def provenance-version 1)
(def provenance-type
  :gesso.assurance.proof.operation-interface-provenance/source-relative-manifest)
(def ^:private identity-id
  :gesso.assurance.proof/source-relative-operation-composition-provenance-v1)

(defn- reject! [reason context]
  (throw (ex-info "Invalid Gesso operation-composition provenance."
                  (merge {:error/type ::invalid-operation-composition-provenance
                          :reason reason}
                         context))))

(defn- sorted-map* [pairs]
  (into (sorted-map-by #(compare (pr-str %1) (pr-str %2))) pairs))

(defn- fingerprint [raw]
  (:fingerprint (subject/describe-subject raw)))

(defn- node-index! [graph]
  ;; The checked dependency graph is already source-relative; retaining all
  ;; nodes/edges here makes provenance auditable rather than hiding unmatched
  ;; source evidence in a report-level fingerprint.
  (let [nodes (:nodes graph)
        node-ids (mapv :id nodes)
        ids (set node-ids)
        root-ids (:roots graph)]
    (when-not (and (vector? nodes)
                   (= (count nodes) (count ids))
                   (= 1 (count root-ids))
                   (set/subset? root-ids ids))
      (reject! :incomplete-dependency-graph {}))
    (let [fingerprints
          (into {} (map (juxt :id (comp fingerprint :subject))) nodes)]
      (doseq [{:keys [id depends-on]} nodes]
        (when-not (and (vector? depends-on)
                       (= (count depends-on) (count (set (map :id depends-on))))
                       (every? (fn [{dependency :id digest :fingerprint}]
                                 (and (contains? ids dependency)
                                      (= digest (get fingerprints dependency))))
                               depends-on))
          (reject! :invalid-source-dependency-edge {:node-id id})))
      (let [entries
            (sorted-map*
             (for [{:keys [id subject depends-on]} nodes]
               [id {:role (get-in subject [:interface :role])
                    :subject-fingerprint (fingerprints id)
                    :dependencies
                    (vec (sort-by (comp str :id) depends-on))}]))]
        {:roots root-ids
         :nodes entries
         :node-count (count nodes)
         :edge-count (reduce + (map (comp count :depends-on) nodes))}))))

(defn- role-index! [graph role source-key expected]
  (let [nodes (filterv #(= role (get-in % [:subject :interface :role]))
                       (:nodes graph))
        ids (mapv #(get-in % [:subject :interface source-key]) nodes)]
    (when-not (and (= expected (set ids))
                   (= (count expected) (count ids)))
      (reject! :incomplete-source-role
               {:role role :expected expected :actual (set ids)}))
    (into {} (map (fn [n]
                    [(get-in n [:subject :interface source-key]) n]))
          nodes)))

(defn- exactly-one-role! [graph role]
  (let [matches (filterv #(= role (get-in % [:subject :interface :role]))
                         (:nodes graph))]
    (when-not (= 1 (count matches))
      (reject! :missing-or-duplicated-source-role
               {:role role :count (count matches)}))
    (first matches)))

(defn- operation-provenance! [graph composition]
  (let [ids (:operation-ids composition)
        operations (role-index! graph :declared-abstract-operation :operation-id ids)
        evidence (role-index! graph :conditional-operation-evidence :operation-id ids)]
    (sorted-map*
     (for [id (sort-by str ids)
           :let [source-node (get operations id)
                 evidence-node (get evidence id)
                 construction (get-in source-node [:subject :interface :construction])
                 contract (get-in composition [:operation-contracts id])
                 raw (fingerprint {:kind :rule :id id
                                   :interface {:original-source construction}})
                 premises (:original-open-obligation-ids contract)]]
       (do
         (when-not (and (= id (:operation-id contract))
                        (= raw (:source-fingerprint contract))
                        (= premises
                           (get-in evidence-node
                                   [:subject :interface :original-open-obligation-ids]))
                        (= premises
                           (get-in composition
                                   [:original-open-obligation-ids-by-operation id]))
                        (= (:catalog-fingerprint composition)
                           (get-in evidence-node
                                   [:subject :interface :source-catalog-fingerprint])))
           (reject! :operation-source-or-premise-mismatch {:operation-id id}))
         [id {:source-node-id (:id source-node)
              :source-node-fingerprint (fingerprint (:subject source-node))
              :source-fingerprint raw
              :transition-fingerprint (:transition-fingerprint contract)
              :subject-fingerprint (:subject-fingerprint contract)
              :evidence-node-id (:id evidence-node)
              :evidence-node-fingerprint (fingerprint (:subject evidence-node))
              :original-open-obligation-ids premises}])))))

(defn- invariant-provenance! [graph composition]
  (let [ids (:invariant-ids composition)
        original (role-index! graph :declared-invariant :invariant-id ids)
        evidence (role-index! graph :conditional-invariant-evidence
                              :invariant-id ids)]
    (sorted-map*
     (for [id (sort-by str ids)
           :let [o (get original id)
                 e (get evidence id)]]
       [id {:source-node-id (:id o)
            :source-node-fingerprint (fingerprint (:subject o))
            :evidence-node-id (:id e)
            :evidence-node-fingerprint (fingerprint (:subject e))
            :induction-status (get-in composition
                                      [:invariant-results id :induction-status])}]))))

(defn describe-composition-provenance!
  "Revalidate untrusted v1443 composition against current original source,
   then build a complete versioned graph/source/assumption lineage report.
   The report is descriptive and does not become an admission token."
  [composition catalog current-graph closure]
  (let [current (comp/require-current-operation-composition!
                 composition catalog current-graph closure)
        summary (app/describe-application-dependencies closure)
        _ (when-not (and (= (:catalog-id summary) (:catalog-id current))
                         (= (:catalog-fingerprint summary)
                            (:catalog-fingerprint current))
                         (= (:induction-fingerprint summary)
                            (:induction-fingerprint current))
                         (= (:operation-ids summary) (:operation-ids current))
                         (= (:invariant-ids summary) (:invariant-ids current))
                         (= (:open-meta-obligations summary)
                            (:open-meta-obligations current))
                         (= (:open-assumptions summary)
                            (:open-assumptions current)))
            (reject! :source-closure-composition-mismatch {}))
        graph-index (node-index! current-graph)
        initial (exactly-one-role! current-graph :shared-initial-condition)
        catalog-node (exactly-one-role! current-graph :operation-catalog)
        ledger-node (exactly-one-role! current-graph :source-derived-induction-ledger)
        assumption-node (exactly-one-role! current-graph :original-open-assumptions)
        app-node (exactly-one-role! current-graph :declared-application-assurance)
        operations (operation-provenance! current-graph current)
        invariants (invariant-provenance! current-graph current)
        lineage
        {:versions {:subject-identity subject/identity-version
                    :application-dependency app/closure-version
                    :operation-interface iface/interface-version
                    :operation-composition comp/composition-version
                    :composition-provenance provenance-version}
         :catalog-id (:catalog-id current)
         :catalog-fingerprint (:catalog-fingerprint current)
         :induction-fingerprint (:induction-fingerprint current)
         :induction-ledger-fingerprint (:ledger-fingerprint summary)
         :operation-interface-fingerprint (:interface-fingerprint current)
         :composition-fingerprint (:fingerprint current)
         :graph graph-index
         :source-roots
         {:catalog {:id (:id catalog-node)
                    :fingerprint (fingerprint (:subject catalog-node))}
          :initial-condition {:id (:id initial)
                              :fingerprint (fingerprint (:subject initial))}
          :ledger {:id (:id ledger-node)
                   :fingerprint (fingerprint (:subject ledger-node))}
          :assumptions {:id (:id assumption-node)
                        :fingerprint (fingerprint (:subject assumption-node))}
          :application {:id (:id app-node)
                        :fingerprint (fingerprint (:subject app-node))}}
         :operations operations
         :invariants invariants
         :original-open-obligation-ids-by-operation
         (:original-open-obligation-ids-by-operation current)
         :open-meta-obligations (:open-meta-obligations current)
         :open-assumptions (:open-assumptions current)}
        identity (subject/describe-subject
                  {:kind :rule :id identity-id :interface lineage})]
    (when-not (= (get-in lineage [:source-roots :application :id])
                 (first (get-in lineage [:graph :roots])))
      (reject! :application-root-mismatch {}))
    {:type provenance-type
     :version provenance-version
     :fingerprint (:fingerprint identity)
     :lineage lineage
     :scope :closed-declared-abstract-machine-only
     :checking :source-relative-graph-and-composition-provenance
     :proof-status :conditional-structural-evidence
     :assumption-discharge :not-performed
     :installed-writer-coverage :not-established
     :implementation-correspondence :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn require-current-composition-provenance!
  "Rebuild and compare the *entire* copyable manifest with current source.
   A matching report is still untrusted diagnostic data, never a proof token."
  [prior composition catalog current-graph closure]
  (let [now (describe-composition-provenance!
             composition catalog current-graph closure)]
    (when-not (and (map? prior) (not (record? prior))
                   (nil? (meta prior)) (= prior now))
      (reject! :stale-or-substituted-composition-provenance
               {:expected (:fingerprint now) :actual (:fingerprint prior)}))
    now))
