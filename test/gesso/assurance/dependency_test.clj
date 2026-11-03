(ns gesso.assurance.dependency-test
  "v1218: adversarial, permanent regression tests for the v1217 STRUCTURAL
   dependency-graph boundary.

   These tests require registered, exact-subject v1215 admission handles;
   enforce canonical DAG identity, dependency fingerprints, and full root
   closure; check snapshot revalidation and reverse dependency impact; and
   refuse forged/checker-looking data. They do NOT certify propositions,
   discharge assumptions, prove application-installation correspondence,
   or prevent hostile trusted-JVM code from replacing private Vars. In
   particular, freshness depends on a genuinely current declaration being
   supplied by a trusted assembly consumer—not untrusted old EDN."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.subject :as subject]))

(defn- operation
  ([id] (operation id 1))
  ([id revision]
   {:kind :operation
    :id id
    :interface {:revision revision
                :guards [:and [:in [:field :request/status]
                                  #{:open :claimed :cancelled}]
                              [:opaque :policy/current]]
                :reads #{:request/status :request/owner}
                :effects [[:set :request/version revision]]}}))

(defn- fingerprint [raw]
  (:fingerprint (subject/describe-subject raw)))

(defn- node [raw dependencies]
  {:id (:id raw)
   :subject raw
   :subject-handle (admission/admit-subject! raw)
   :depends-on (mapv (fn [target]
                       {:id (:id target) :fingerprint (fingerprint target)})
                     dependencies)})

(defn- fixture-graph
  "Six nodes, two business branches, a shared policy, and an audit branch.
   Dependency order: member -> eligible -> {claim,cancel}, audit -> root."
  []
  (let [member (operation :policy/member)
        eligible (operation :policy/eligible)
        claim (operation :operation/claim)
        cancel (operation :operation/cancel)
        audit (operation :policy/audit)
        root (operation :application/root)]
    {:nodes [(node member [])
             (node eligible [member])
             (node claim [eligible member])
             (node cancel [eligible])
             (node audit [])
             (node root [claim cancel audit])]
     :roots #{:application/root}}))

(defn- node-index [graph id]
  (first (keep-indexed (fn [i node]
                         (when (= id (:id node)) i))
                       (:nodes graph))))

(defn- change-node
  "Change a subject plus its handle, but deliberately DO NOT repair edges."
  [graph id revision]
  (let [index (node-index graph id)
        old (get-in graph [:nodes index])
        raw (operation id revision)]
    (assoc-in graph [:nodes index]
              (assoc old :subject raw
                         :subject-handle (admission/admit-subject! raw)))))

(defn- rewrite-reference
  [graph source target target-raw]
  (update-in graph [:nodes (node-index graph source) :depends-on]
             (fn [edges]
               (mapv (fn [edge]
                       (if (= (:id edge) target)
                         (assoc edge :fingerprint (fingerprint target-raw))
                         edge))
                     edges))))

(defn- ex-data-from [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects!
  [thunk error-type reason]
  (let [data (ex-data-from thunk)]
    (is (map? data) (str "Expected typed rejection: " reason))
    (is (= error-type (:error/type data)))
    (is (= reason (:reason data)))))

(defn- rejects-graph! [thunk reason]
  (rejects! thunk :gesso.assurance.dependency/invalid-dependency-graph reason))

(defn- rejects-admission! [thunk reason]
  (rejects! thunk :gesso.assurance.admission/invalid-admission reason))

(defn- rejects-subject! [thunk reason]
  (rejects! thunk :gesso.assurance.subject/invalid-semantic-subject reason))

(deftest valid-dag-has-exact-subject-and-edge-snapshots
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        description (dependency/describe-graph handle)
        nodes (:nodes description)]
    (is (dependency/checked-graph? handle))
    (is (identical? handle (dependency/require-current-graph! handle graph)))
    (is (= dependency/graph-type (:type description)))
    (is (= dependency/graph-version (:version description)))
    (is (= #{:application/root} (:roots description)))
    (is (= (set (map :id (:nodes graph))) (set (keys nodes))))
    (is (= (set (keys nodes)) (set (:dependency-order description))))
    (is (= 6 (count nodes)))
    (doseq [[id entry] nodes]
      (let [original (get-in graph [:nodes (node-index graph id)])]
        (is (= id (:id entry)))
        (is (= (fingerprint (:subject original)) (:fingerprint entry)))
        (is (= (set (:depends-on original)) (set (:depends-on entry))))
        (is (= (count (:depends-on original)) (count (:depends-on entry))))))))

(deftest dependency-order-is-topological-for-every-edge
  (let [description (dependency/describe-graph
                     (dependency/check-graph! (fixture-graph)))
        positions (zipmap (:dependency-order description) (range))]
    (doseq [[parent entry] (:nodes description)
            edge (:depends-on entry)]
      (let [target (:id edge)]
        (is (< (positions target) (positions parent))
            (str target " must precede " parent))))))

(deftest node-and-edge-order-do-not-affect-snapshot-identity
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        reordered (-> graph
                      (update :nodes
                              (fn [nodes]
                                (mapv #(update % :depends-on
                                               (comp vec reverse))
                                      (reverse nodes)))))]
    (is (identical? handle
                    (dependency/require-current-graph! handle reordered)))
    (is (= (dependency/describe-graph handle)
           (dependency/describe-graph (dependency/check-graph! reordered))))))

(deftest semantically-equivalent-edn-integers-preserve-identity
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        alternative
        (update graph :nodes
                (fn [nodes]
                  (mapv (fn [n]
                          (let [same (update-in (:subject n)
                                                [:interface :revision]
                                                bigint)]
                            (assoc n :subject same
                                     :subject-handle
                                     (admission/admit-subject! same))))
                        nodes)))]
    (is (= (fingerprint (operation :policy/member 1))
           (fingerprint (operation :policy/member 1N))))
    (is (identical? handle
                    (dependency/require-current-graph! handle alternative)))))

(deftest a-checker-issued-graph-has-no-proof-or-runtime-authority
  (let [handle (dependency/check-graph! (fixture-graph))
        description (dependency/describe-graph handle)]
    (is (= :structural-only (:checking description)))
    (is (= :not-proved (:proof-status description)))
    (is (= :not-established (:assumption-discharge description)))
    (is (= :not-established (:implementation-correspondence description)))
    (is (= :not-a-proof (:admission description)))
    (is (= :none (:authority description)))
    (doseq [claimed-authority [:certificate :kernel-checked :valid?
                              :proved? :runtime-authorized :discharged]]
      (is (not (contains? description claimed-authority))))
    (is (not (dependency/checked-graph? description)))))

(deftest arbitrary-maps-and-forged-checker-results-never-become-handles
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        summary (dependency/describe-graph handle)
        serialized (edn/read-string (pr-str summary))
        variants [nil false true 0 :valid "checked" [] #{}
                  (Object.) graph summary (into {} summary) serialized
                  (assoc summary :checking :kernel-checked)
                  (assoc summary :proof-status :proved)
                  (assoc summary :assumption-discharge :discharged)
                  (assoc summary :implementation-correspondence :established)
                  (assoc summary :admission :certified)
                  (assoc summary :authority :proof)
                  (assoc summary :certificate {:valid? true})
                  {:type dependency/graph-type
                   :version dependency/graph-version
                   :valid? true}]]
    (is (= summary serialized))
    (doseq [candidate variants]
      (testing (str "Forged or serialized dependency graph: "
                    (some-> candidate class str))
        (is (not (dependency/checked-graph? candidate)))
        (rejects-graph! #(dependency/describe-graph candidate)
                        :unrecognized-graph)
        (rejects-graph! #(dependency/require-current-graph! candidate graph)
                        :unrecognized-graph)
        (rejects-graph! #(dependency/affected-nodes candidate #{:policy/member})
                        :unrecognized-graph)))
    (is (dependency/checked-graph? handle))))

(deftest manually-constructed-object-does-not-bypass-registration
  (let [graph (fixture-graph)
        genuine (dependency/check-graph! graph)
        imposter (gesso.assurance.dependency.CheckedGraph.)]
    (is (instance? gesso.assurance.dependency.CheckedGraph genuine))
    (is (instance? gesso.assurance.dependency.CheckedGraph imposter))
    (is (not (identical? genuine imposter)))
    (is (dependency/checked-graph? genuine))
    (is (not (dependency/checked-graph? imposter)))
    (rejects-graph! #(dependency/describe-graph imposter) :unrecognized-graph)
    (rejects-graph! #(dependency/require-current-graph! imposter graph)
                    :unrecognized-graph)))

(deftest summary-does-not-contain-live-admission-handles
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        summary (dependency/describe-graph handle)]
    (is (every? (fn [[_ node]]
                  (not (contains? node :subject-handle)))
                (:nodes summary)))
    (is (every? (fn [[_ node]]
                  (every? #(= #{:id :fingerprint} (set (keys %)))
                          (:depends-on node)))
                (:nodes summary)))
    (is (= summary (edn/read-string (pr-str summary))))
    (rejects-graph! #(dependency/describe-graph summary)
                    :unrecognized-graph)))

(deftest graph-and-node-unknown-fields-are-rejected
  (let [graph (fixture-graph)]
    (doseq [changed [(assoc graph :valid? true)
                     (assoc graph :proof {:valid? true})
                     (assoc graph :checking :kernel-checked)
                     (dissoc graph :roots)
                     (dissoc graph :nodes)]]
      (rejects-graph! #(dependency/check-graph! changed) :wrong-keys))
    (doseq [changed [(assoc-in graph [:nodes 0 :valid?] true)
                     (assoc-in graph [:nodes 0 :proof-status] :proved)
                     (assoc-in graph [:nodes 0 :fingerprint] (fingerprint (operation :policy/member)))
                     (update-in graph [:nodes 0] dissoc :depends-on)
                     (update-in graph [:nodes 0] dissoc :subject-handle)]]
      (rejects-graph! #(dependency/check-graph! changed) :wrong-keys))))

(deftest graph-rejects-unexpected-collection-types
  (let [graph (fixture-graph)]
    (doseq [invalid [nil [] () {} #{} 1 "nodes" (seq (:nodes graph))]]
      (rejects-graph! #(dependency/check-graph! (assoc graph :nodes invalid))
                      :invalid-nodes))
    (doseq [invalid [nil #{} [] () {} false :policy/member
                     #{:unqualified} #{"root"}]]
      (rejects-graph! #(dependency/check-graph! (assoc graph :roots invalid))
                      :invalid-roots))
    (rejects-graph!
     #(dependency/check-graph!
       (assoc-in graph [:nodes (node-index graph :operation/claim) :depends-on]
                 '({:id :policy/eligible :fingerprint "sha256:placeholder"})))
     :invalid-dependencies)))

(deftest duplicate-nodes-and-dependencies-rejected-before-issuance
  (let [graph (fixture-graph)
        claim-index (node-index graph :operation/claim)
        edge (first (get-in graph [:nodes claim-index :depends-on]))]
    (rejects-graph!
     #(dependency/check-graph! (update graph :nodes conj (first (:nodes graph))))
     :duplicate-node)
    (rejects-graph!
     #(dependency/check-graph!
       (update-in graph [:nodes claim-index :depends-on] conj edge))
     :duplicate-dependency)))

(deftest every-edge-must-name-an-existing-correct-fingerprint
  (let [graph (fixture-graph)
        claim-index (node-index graph :operation/claim)
        pos [:nodes claim-index :depends-on 0]]
    (rejects-graph!
     #(dependency/check-graph! (assoc-in graph (conj pos :id) :policy/missing))
     :missing-dependency)
    (rejects-graph!
     #(dependency/check-graph!
       (assoc-in graph (conj pos :fingerprint)
                 (fingerprint (operation :policy/eligible 2))))
     :stale-dependency)
    (doseq [bad [nil 0 :sha256 "sha256:1234" (str "sha256:" (apply str (repeat 64 "X")))
                 (str "sha256:" (apply str (repeat 63 "a")))]]
      (rejects-graph!
       #(dependency/check-graph! (assoc-in graph (conj pos :fingerprint) bad))
       :invalid-dependency-fingerprint))))

(deftest dependency-edges-refuse-alleged-certification-data
  (let [graph (fixture-graph)
        pos [:nodes (node-index graph :operation/claim) :depends-on 0]]
    (doseq [field [:valid? :certified? :proof :checker-result :assumption-discharge
                   :runtime-verified :authority]]
      (rejects-graph!
       #(dependency/check-graph! (assoc-in graph (conj pos field) true))
       :wrong-keys))
    (rejects-graph!
     #(dependency/check-graph! (update-in graph pos dissoc :fingerprint))
     :wrong-keys)))

(deftest self-references-and-long-cycles-fail-closed
  (let [graph (fixture-graph)
        member-index (node-index graph :policy/member)
        member (operation :policy/member)
        root (operation :application/root)
        circular (assoc-in graph [:nodes member-index :depends-on]
                           [{:id :application/root :fingerprint (fingerprint root)}])
        self (assoc-in graph [:nodes member-index :depends-on]
                       [{:id :policy/member :fingerprint (fingerprint member)}])]
    (rejects-graph! #(dependency/check-graph! circular) :cycle)
    (rejects-graph! #(dependency/check-graph! self) :self-dependency)
    ;; Two-node cycle: eligible -> claim -> eligible.
    (let [eligible-index (node-index graph :policy/eligible)
          two-cycle (update-in graph [:nodes eligible-index :depends-on]
                               conj {:id :operation/claim
                                     :fingerprint
                                     (fingerprint (operation :operation/claim))})]
      (rejects-graph! #(dependency/check-graph! two-cycle) :cycle))))

(deftest a-missing-root-or-unreachable-node-is-not-a-complete-scope
  (let [graph (fixture-graph)]
    (rejects-graph!
     #(dependency/check-graph! (assoc graph :roots #{:application/missing}))
     :missing-root)
    (rejects-graph!
     #(dependency/check-graph! (assoc graph :roots #{:policy/member}))
     :unreachable-node)
    (rejects-graph!
     #(dependency/check-graph! (update graph :nodes conj
                                       (node (operation :operation/hidden) [])))
     :unreachable-node)))

(deftest multiple-roots-are-allowed-only-when-all-nodes-are-covered
  (let [left (operation :demo/left)
        right (operation :demo/right)
        graph {:nodes [(node right []) (node left [])]
               :roots #{:demo/left :demo/right}}
        handle (dependency/check-graph! graph)
        description (dependency/describe-graph handle)]
    (is (= #{:demo/left :demo/right} (:roots description)))
    (is (= [:demo/left :demo/right] (:dependency-order description)))
    (is (= #{:demo/left} (dependency/affected-nodes handle #{:demo/left})))
    (is (= #{:demo/right} (dependency/affected-nodes handle #{:demo/right})))
    (rejects-graph!
     #(dependency/check-graph! (assoc graph :roots #{:demo/left}))
     :unreachable-node)))

(deftest a-node-name-is-not-a-semantic-identity
  (let [graph (fixture-graph)
        index (node-index graph :policy/member)
        old (get-in graph [:nodes index])
        other (operation :policy/member 2)]
    (is (= (:id other) (:id (:subject old))))
    (is (not= (fingerprint other) (fingerprint (:subject old))))
    (rejects-admission!
     #(dependency/check-graph!
       (assoc-in graph [:nodes index :subject] other))
     :wrong-subject)
    (rejects-graph!
     #(dependency/check-graph!
       (assoc-in graph [:nodes index :id] :policy/not-member))
     :node-subject-id-mismatch)))

(deftest forgery-in-a-node-subject-is-never-treated-as-assurance
  (let [graph (fixture-graph)
        index (node-index graph :policy/member)]
    (doseq [field [:valid? :proof :certificate :fingerprint :proved?]]
      (rejects-subject!
       #(dependency/check-graph!
         (assoc-in graph [:nodes index :subject field] true))
       :wrong-keys))))

(deftest missing-forged-and-wrong-kind-subject-handles-fail-closed
  (let [graph (fixture-graph)
        index (node-index graph :policy/member)
        original (get-in graph [:nodes index :subject])
        another (operation :policy/eligible)
        incorrect (admission/admit-subject! another)
        forged (gesso.assurance.admission.AdmissionHandle.)]
    (doseq [bad [nil false {} :accepted forged]]
      (rejects-admission!
       #(dependency/check-graph!
         (assoc-in graph [:nodes index :subject-handle] bad))
       :unrecognized-handle))
    (rejects-admission!
     #(dependency/check-graph!
       (assoc-in graph [:nodes index :subject-handle] incorrect))
     :wrong-subject)
    (let [premise {:id :policy/member-current
                   :subject original
                   :scope {:kind :operation :id :policy/member}
                   :proposition [:member/current :actor]
                   :parameters {}
                   :semantic-context {}
                   :dependencies #{}
                   :origin :runtime
                   :implementation-boundary nil}
          matching (admission/admit-premise-match! premise premise)]
      (rejects-admission!
       #(dependency/check-graph!
         (assoc-in graph [:nodes index :subject-handle] matching))
       :wrong-check-kind))))

(deftest stale-dependency-detected-before-stale-graph-comparison
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        newer (change-node graph :policy/member 2)
        repaired (-> newer
                     (rewrite-reference :policy/eligible :policy/member
                                        (operation :policy/member 2))
                     (rewrite-reference :operation/claim :policy/member
                                        (operation :policy/member 2)))]
    (rejects-graph!
     #(dependency/require-current-graph! handle newer)
     :stale-dependency)
    (rejects-graph!
     #(dependency/require-current-graph! handle repaired)
     :stale-graph)
    (is (= #{:policy/member :policy/eligible :operation/claim
             :operation/cancel :application/root}
           (dependency/affected-nodes handle #{:policy/member})))
    (is (identical? handle
                    (dependency/require-current-graph! handle graph)))))

(deftest changing-root-scope-invalidates-a-previous-snapshot
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        expanded (assoc graph :roots #{:application/root :policy/member})]
    (is (dependency/checked-graph? (dependency/check-graph! expanded)))
    (rejects-graph!
     #(dependency/require-current-graph! handle expanded)
     :stale-graph)
    (is (identical? handle (dependency/require-current-graph! handle graph)))))

(deftest adding-and-removing-nodes-requires-rechecking-identity
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        extra (operation :policy/extra)
        expanded (-> graph
                     (update :nodes conj (node extra []))
                     (update-in [:nodes (node-index graph :application/root)
                                 :depends-on]
                                conj {:id :policy/extra
                                      :fingerprint (fingerprint extra)}))
        shrunk (update graph :nodes
                       (fn [nodes]
                         (mapv (fn [n]
                                 (if (= (:id n) :application/root)
                                   (update n :depends-on
                                           (fn [edges]
                                             (vec (remove
                                                   #(= :policy/audit (:id %))
                                                   edges))))
                                   n))
                               (remove #(= :policy/audit (:id %)) nodes))))]
    (is (dependency/checked-graph? (dependency/check-graph! expanded)))
    (is (dependency/checked-graph? (dependency/check-graph! shrunk)))
    (rejects-graph! #(dependency/require-current-graph! handle expanded)
                    :stale-graph)
    (rejects-graph! #(dependency/require-current-graph! handle shrunk)
                    :stale-graph)))

(deftest transitive-reverse-closure-is-exact
  (let [handle (dependency/check-graph! (fixture-graph))
        expected {#{:policy/member}
                  #{:policy/member :policy/eligible :operation/claim
                    :operation/cancel :application/root}
                  #{:policy/eligible}
                  #{:policy/eligible :operation/claim :operation/cancel
                    :application/root}
                  #{:operation/claim}
                  #{:operation/claim :application/root}
                  #{:operation/cancel}
                  #{:operation/cancel :application/root}
                  #{:policy/audit}
                  #{:policy/audit :application/root}
                  #{:application/root}
                  #{:application/root}
                  #{:policy/audit :operation/claim}
                  #{:policy/audit :operation/claim :application/root}}]
    (doseq [[changed closure] expected]
      (is (= closure (dependency/affected-nodes handle changed))))))

(deftest impact-query-refuses-unknown-and-ill-formed-change-sets
  (let [handle (dependency/check-graph! (fixture-graph))]
    (doseq [bad [nil #{} [] () {} true 0 :policy/member
                 #{:policy/member :policy/missing}
                 #{:member}
                 #{"member"}
                 #{nil}]]
      (let [reason (if (= bad #{:policy/member :policy/missing})
                     :unknown-changed-node
                     :invalid-changed-nodes)]
        (rejects-graph! #(dependency/affected-nodes handle bad) reason)))
    (is (= #{:policy/member :policy/eligible :operation/claim
             :operation/cancel :application/root}
           (dependency/affected-nodes handle #{:policy/member})))))

(deftest graph-size-limits-reject-before-any-authority-is-issued
  (let [graph (fixture-graph)
        many (assoc graph :nodes (vec (repeat 513 (first (:nodes graph)))))]
    (rejects-graph! #(dependency/check-graph! many) :node-limit)
    ;; 93 fully connected, acyclic nodes have 4,278 edges (> 4,096).
    ;; All nodes are reachable from the final root, and references have valid
    ;; fingerprints. The edge limit is checked before expensive graph walking.
    (let [subjects (mapv #(operation (keyword "large" (str "n" %))) (range 93))
          fingerprints (mapv fingerprint subjects)
          nodes (mapv
                 (fn [i]
                   {:id (:id (subjects i))
                    :subject (subjects i)
                    :subject-handle (admission/admit-subject! (subjects i))
                    :depends-on
                    (mapv (fn [j]
                            {:id (:id (subjects j))
                             :fingerprint (fingerprints j)})
                          (range i))})
                 (range (count subjects)))
          massive {:nodes nodes :roots #{(:id (last subjects))}}]
      (is (= 4278 (reduce + (map (comp count :depends-on) nodes))))
      (rejects-graph! #(dependency/check-graph! massive) :edge-limit))))

(deftest rejected-constructions-do-not-taint-existing-checked-graphs
  (let [graph (fixture-graph)
        handle (dependency/check-graph! graph)
        malformed (assoc-in graph [:nodes 0 :valid?] true)]
    (rejects-graph! #(dependency/check-graph! malformed) :wrong-keys)
    (is (dependency/checked-graph? handle))
    (is (identical? handle
                    (dependency/require-current-graph! handle graph)))
    (is (= :not-proved
           (:proof-status (dependency/describe-graph handle))))))
