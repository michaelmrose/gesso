(ns gesso.assurance.proof.induction-dependency-test
  "v1283: permanent adversarial regression tests for source-relative induction
   dependency closure (v1282). The exact graph must be derived from ORIGINAL
   declarations, not the portion of a generic DAG the caller happens to give.

   This suite exercises identity/edge closure, stale source, forged metadata,
   copied handles, malformed or substituted DAGs, original open obligations,
   and scope restrictions. It does not establish checker metatheory, physical
   writer coverage, or universal implementation correspondence."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.induction-dependency :as deps]
            [gesso.assurance.subject :as subject]))

(def ^:private state-types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/version {:type :integer}
   :request/owner {:type :keyword :nullable? true}})

(def ^:private version-invariant
  {:readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})

(def ^:private domain-invariant
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed :done}]]})

(def ^:private true-invariant
  {:readers #{} :expression [:literal true]})

(def ^:private initial-condition
  {:readers #{:request/status :request/version}
   :expression [:and (:expression domain-invariant)
                (:expression version-invariant)]})

(defn- operation [id from to]
  {:subject {:kind :operation :id id :interface {:operation id}}
   :state-types state-types
   :input-types {}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal from]]}
   :reads #{:request/status}
   :writes #{:request/status}
   :replacements {:request/status [:literal to]}
   :frame #{:request/version :request/owner}})

(def ^:private catalog
  {:id :request/workflow
   :declared-operation-ids #{:request/claim :request/unclaim}
   :initial-condition initial-condition
   :invariants {:request/domain domain-invariant
                :request/version-nonnegative version-invariant
                :request/truth true-invariant}
   :operations [{:id :request/claim
                 :construction (operation :request/claim :open :claimed)}
                {:id :request/unclaim
                 :construction (operation :request/unclaim :claimed :open)}]})

(defn- fresh-graph [] (deps/build-induction-dependency-graph! catalog))

(defn- node-with-role [graph role]
  (first (filter #(= role (get-in % [:subject :interface :role]))
                 (:nodes graph))))

(defn- node-index [graph id]
  (first (keep-indexed (fn [i n] (when (= id (:id n)) i))
                       (:nodes graph))))

(defn- update-node [graph id f]
  (update-in graph [:nodes (node-index graph id)] f))

(defn- fingerprint [raw]
  (:fingerprint (subject/describe-subject raw)))

(defn- source-node [id]
  {:kind :rule :id id :interface {:role :invented}})

(defn- extra-node []
  (let [raw (source-node :forgery/extra)]
    {:id (:id raw)
     :subject raw
     :subject-handle (admission/admit-subject! raw)
     :depends-on []}))

(defn- data-from [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [f expected-type expected-reason]
  (let [data (data-from f)]
    (is (map? data) (str "Expected typed rejection: " expected-reason))
    (is (= expected-type (:error/type data)) (pr-str data))
    (is (= expected-reason (:reason data)) (pr-str data))
    data))

(defn- reject-closure! [f reason]
  (reject! f :gesso.assurance.proof.induction-dependency/invalid-induction-dependencies
           reason))

(defn- checked [graph]
  (deps/check-induction-dependencies! catalog graph))

(def ^:private required-external
  #{:induction-rule-soundness
    :initial-state-existence
    :external-initializer-coverage
    :external-operation-set-completeness
    :physical-write-path-coverage
    :implementation-correspondence
    :checker-meta-soundness})

(deftest complete-catalog-produces-single-root-exact-graph
  (let [graph (fresh-graph)
        nodes (:nodes graph)
        catalog-node (node-with-role graph :operation-catalog)
        init-node (node-with-role graph :shared-initial-condition)
        invariant-nodes (filter #(= :declared-invariant
                                    (get-in % [:subject :interface :role])) nodes)
        operation-nodes (filter #(= :declared-abstract-operation
                                    (get-in % [:subject :interface :role])) nodes)]
    (is (= #{:roots :nodes} (set (keys graph))))
    (is (= 8 (count nodes)))
    (is (= 8 (count (set (map :id nodes)))))
    (is (= 1 (count (:roots graph))))
    (is (= #{(:id catalog)} (:roots graph)))
    (is (= :conditional-induction
           (get-in (first nodes) [:subject :interface :proof-kind])))
    (is (= 1 (count (filter #(contains? (:roots graph) (:id %)) nodes))))
    (is (some? catalog-node))
    (is (some? init-node))
    (is (= 3 (count invariant-nodes)))
    (is (= 2 (count operation-nodes)))
    (is (= (:id catalog-node) (-> nodes first :depends-on first :id)))
    (is (= #{:request/claim :request/unclaim}
           (set (map #(get-in % [:subject :interface :operation-id]) operation-nodes))))
    (is (= #{:request/domain :request/version-nonnegative :request/truth}
           (set (map #(get-in % [:subject :interface :invariant-id]) invariant-nodes))))
    (is (= 6 (count (:depends-on catalog-node))))
    (is (every? #(= 4 (count (:depends-on %))) operation-nodes))
    (is (every? empty? (map :depends-on (conj (vec invariant-nodes) init-node))))
    (is (= 15 (reduce + (map (comp count :depends-on) nodes))))))

(deftest closure-handle-is-local-authentic-and-current
  (let [graph (fresh-graph)
        h (checked graph)
        report (deps/describe-induction-dependencies h)]
    (is (deps/checked-induction-dependencies? h))
    (is (not (deps/checked-induction-dependencies? report)))
    (is (identical? h (deps/require-current-induction-dependencies!
                       h catalog graph)))
    (is (= deps/dependency-type (:type report)))
    (is (= deps/dependency-version (:version report)))
    (is (= :original-proof-source-and-exact-dependency-graph
           (:checking report)))
    (is (= :source-dependencies-structurally-checked (:proof-status report)))
    (is (= (:id catalog) (:catalog-id report)))
    (is (= 8 (:node-count report)))
    (is (= 15 (:edge-count report)))
    (is (= #{:request/claim :request/unclaim} (:operation-ids report)))
    (is (= (set (keys (:invariants catalog))) (:invariant-ids report)))
    (is (string? (:fingerprint report)))
    (is (re-matches #"sha256:[a-f0-9]{64}" (:fingerprint report)))
    (is (not (contains? report :graph-handle)))
    (is (not (contains? report :graph-snapshot)))))

(deftest graph-node-and-edge-order-can-change-without-changing-closure
  (let [g (fresh-graph)
        h (checked g)
        reordered (update g :nodes
                          (fn [nodes]
                            (mapv #(update % :depends-on (comp vec reverse))
                                  (reverse nodes))))]
    (is (identical? h (deps/require-current-induction-dependencies!
                       h catalog reordered)))
    (is (deps/checked-induction-dependencies?
         (deps/check-induction-dependencies! catalog reordered)))))

(deftest dependency-identities-are-not-reused-across-roles
  (let [g (fresh-graph)
        all-ids (map :id (:nodes g))]
    (is (= (count all-ids) (count (set all-ids))))
    (is (every? qualified-keyword? all-ids))
    (is (every? #(= "gesso.assurance.proof.induction-dependency"
                    (namespace (:id %)))
                (remove #(contains? (:roots g) (:id %)) (:nodes g))))
    (is (not= (:id (node-with-role g :shared-initial-condition))
              (:id (node-with-role g :operation-catalog))))))

(deftest generic-valid-subset-is-not-valid-induction-closure
  (let [g (fresh-graph)
        root (first (:nodes g))
        shrunk {:roots (:roots g)
                :nodes [(assoc root :depends-on [])]}
        generic (dependency/check-graph! shrunk)]
    (is (dependency/checked-graph? generic))
    (reject-closure! #(checked shrunk) :source-dependency-coverage-mismatch)
    (is (not (deps/checked-induction-dependencies? generic)))))

(deftest each-omitted-node-is-rejected-even-with-valid-other-nodes
  (let [g (fresh-graph)]
    (doseq [n (:nodes g)]
      (let [changed (update g :nodes
                            (fn [nodes] (vec (remove #(= (:id %) (:id n)) nodes))))
            data (reject-closure! #(checked changed)
                                  :source-dependency-coverage-mismatch)]
        (is (contains? (:missing data) (:id n)))))))

(deftest extra-and-duplicate-nodes-are-not-accepted
  (let [g (fresh-graph)
        extra (update g :nodes conj (extra-node))
        duplicate (update g :nodes conj (first (:nodes g)))]
    (is (= #{:forgery/extra}
           (:unexpected (reject-closure! #(checked extra)
                                         :source-dependency-coverage-mismatch))))
    (reject-closure! #(checked duplicate) :source-dependency-coverage-mismatch)))

(deftest missing-duplicate-and-substituted-roots-are-rejected
  (let [g (fresh-graph)]
    (doseq [roots [#{} #{:forgery/root} (conj (:roots g) :forgery/root)]]
      (reject-closure! #(checked (assoc g :roots roots))
                       :source-dependency-coverage-mismatch))))

(deftest removing-or-forging-edges-invalidates-exact-source-closure
  (let [g (fresh-graph)
        root-id (first (:roots g))
        catalog-id (:id (node-with-role g :operation-catalog))
        op-id (:id (first (filter #(= :declared-abstract-operation
                                     (get-in % [:subject :interface :role]))
                                  (:nodes g))))
        initial-id (:id (node-with-role g :shared-initial-condition))
        cases [(update-node g root-id #(assoc % :depends-on []))
               (update-node g catalog-id #(update % :depends-on (comp vec rest)))
               (update-node g op-id #(update % :depends-on (comp vec rest)))
               (update-node g op-id #(update % :depends-on conj
                                                    {:id :forgery/missing
                                                     :fingerprint (fingerprint
                                                                   (source-node
                                                                    :forgery/missing))}))]]
    (doseq [changed cases]
      (reject-closure! #(checked changed) :source-dependency-content-mismatch))
    ;; Canonical edge sets are identical when one edge is repeated, but the
    ;; generic checker rejects the duplicate rather than accepting the graph.
    (reject! #(checked (update-node g catalog-id
                                    (fn [node]
                                      (update node :depends-on conj
                                              {:id initial-id
                                               :fingerprint
                                               (fingerprint
                                                (:subject (node-with-role
                                                           g :shared-initial-condition)))}))))
             :gesso.assurance.dependency/invalid-dependency-graph
             :duplicate-dependency)))

(deftest forged-edge-fingerprints-never-substitute-for-original-source
  (let [g (fresh-graph)
        root-id (first (:roots g))
        altered (update-node g root-id
                             #(assoc-in % [:depends-on 0 :fingerprint]
                                        (str "sha256:" (apply str (repeat 64 "0")))))]
    (reject-closure! #(checked altered) :source-dependency-content-mismatch)))

(deftest reordered-or-replaced-references-are-exactly-checked
  (let [g (fresh-graph)
        catalog-id (:id (node-with-role g :operation-catalog))
        edges (:depends-on (get-in g [:nodes (node-index g catalog-id)]))
        edited (update-node g catalog-id
                            #(assoc % :depends-on
                                    (assoc edges 0 (assoc (first edges) :id
                                                          :forgery/other))))]
    (reject-closure! #(checked edited) :source-dependency-content-mismatch)))

(deftest node-source-substitution-even-with-fresh-admission-fails
  (let [g (fresh-graph)
        original (node-with-role g :operation-catalog)
        id (:id original)
        forged (assoc-in (:subject original) [:interface :operation-ids]
                         #{:request/claim})
        altered (update-node g id
                             #(assoc % :subject forged
                                     :subject-handle
                                     (admission/admit-subject! forged)))]
    (reject-closure! #(checked altered) :source-dependency-content-mismatch)))

(deftest swapped-live-subject-handle-cannot-pass-generic-authentication
  (let [g (fresh-graph)
        catalog-id (:id (node-with-role g :operation-catalog))
        replacement (admission/admit-subject! (source-node :forgery/substitute))
        altered (update-node g catalog-id #(assoc % :subject-handle replacement))]
    (reject! #(checked altered)
             :gesso.assurance.admission/invalid-admission
             :wrong-subject)))

(deftest graph-and-node-shapes-are-not-open-records
  (let [g (fresh-graph)
        id (:id (first (:nodes g)))
        malformed [(assoc g :valid? true)
                   (dissoc g :roots)
                   (assoc g :nodes (seq (:nodes g)))
                   (with-meta g {:verified true})
                   (assoc g :roots (with-meta (:roots g) {:approved true}))
                   (assoc g :nodes (with-meta (:nodes g) {:approved true}))]]
    (doseq [changed malformed]
      (reject-closure! #(checked changed) :invalid-graph-shape))
    (doseq [changed [(update-node g id #(assoc % :proved? true))
                     (update-node g id #(dissoc % :subject))
                     (update-node g id #(with-meta % {:approved true}))
                     (update-node g id #(update % :depends-on seq))
                     (update-node g id #(update % :depends-on
                                               (fn [edges] (with-meta edges {:approved true}))))
                     (update-node g id #(update-in % [:depends-on 0]
                                                   (fn [edge] (assoc edge :valid? true))))
                     (update-node g id #(update-in % [:depends-on 0]
                                                   (fn [edge] (with-meta edge {:approved true}))))]]
      (reject-closure! #(checked changed) :invalid-node-shape))))

(deftest copied-reports-and-pretend-handles-have-no-authority
  (let [g (fresh-graph)
        h (checked g)
        report (deps/describe-induction-dependencies h)
        copied (edn/read-string (pr-str report))
        forged (assoc report :proof-status :proved
                      :authority :runtime-authorized
                      :certificate :admitted
                      :assumption-discharge :complete)]
    (doseq [fake [nil false :checked 0 {} [] (Object.) g report copied forged
                  (dependency/check-graph! g)
                  (certificate/induction-certificate-subject! catalog)]]
      (is (not (deps/checked-induction-dependencies? fake)))
      (reject-closure! #(deps/describe-induction-dependencies fake)
                       :unrecognized-dependency-handle)
      (reject-closure! #(deps/require-current-induction-dependencies!
                        fake catalog g)
                       :unrecognized-dependency-handle))))

(deftest copied-induction-result-does-not-replace-original-catalog
  (let [g (fresh-graph)
        report (induction/check-induction! catalog)]
    (is (some? (:fingerprint report)))
    (is (some? (data-from #(deps/check-induction-dependencies! report g))))
    (is (some? (data-from #(deps/build-induction-dependency-graph! report))))))

(deftest source-change-invalidates-live-handle-before-graph-reuse
  (let [g (fresh-graph)
        h (checked g)
        changed [(assoc catalog :id :request/renamed-workflow)
                 (update catalog :invariants dissoc :request/truth)
                 (assoc-in catalog [:operations 0 :construction :subject :interface :revision] 2)
                 (assoc catalog :initial-condition
                        (assoc initial-condition :expression
                               [:and (:expression version-invariant)
                                (:expression domain-invariant)]))
                 (-> catalog
                     (update :declared-operation-ids conj :request/refresh)
                     (update :operations conj
                             {:id :request/refresh
                              :construction (operation :request/refresh
                                                       :open :open)}))]]
    (doseq [c changed]
      (reject-closure! #(deps/require-current-induction-dependencies!
                        h c g)
                       :stale-source-dependency-closure))))

(deftest original-graph-from-one-catalog-not-valid-for-a-different-catalog
  (let [g (fresh-graph)
        changed (assoc catalog :id :request/renamed-workflow)]
    (reject-closure! #(deps/check-induction-dependencies! changed g)
                     :source-dependency-coverage-mismatch)))

(deftest current-graph-mutations-invalidate-registered-closure
  (let [g (fresh-graph)
        h (checked g)
        catalog-id (:id (node-with-role g :operation-catalog))
        versions [(update-node g catalog-id #(assoc-in %
                                                       [:subject :interface :catalog-id]
                                                       :request/not-the-catalog))
                  (update-node g catalog-id #(assoc % :depends-on []))
                  (update g :nodes (comp vec rest))]]
    (doseq [changed versions]
      (reject-closure! #(deps/require-current-induction-dependencies!
                        h catalog changed)
                       :source-dependency-content-mismatch))))

(deftest current-graph-with-swapped-handle-is-not-current
  (let [g (fresh-graph)
        h (checked g)
        id (:id (node-with-role g :operation-catalog))
        altered (update-node g id #(assoc % :subject-handle
                                          (admission/admit-subject!
                                           (source-node :forgery/other))))]
    (reject! #(deps/require-current-induction-dependencies! h catalog altered)
             :gesso.assurance.admission/invalid-admission
             :wrong-subject)))

(deftest induction-open-obligations-are-preserved-exactly
  (let [g (fresh-graph)
        r (induction/check-induction! catalog)
        description (deps/describe-induction-dependencies (checked g))]
    (is (= (:open-meta-obligations r)
           (:open-meta-obligations description)))
    (is (= (:original-open-obligation-ids-by-operation r)
           (:original-open-obligation-ids-by-operation description)))
    (is (= (:fingerprint r) (:induction-fingerprint description)))
    (is (= (:catalog-fingerprint r) (:catalog-fingerprint description)))
    (is (set/subset? required-external
                     (:open-external-obligations description)))
    (is (= :not-performed (:assumption-discharge description)))
    (is (= :not-established (:implementation-correspondence description)))
    (is (= :not-established (:runtime-enforcement description)))
    (is (= :not-issued (:certificate description)))
    (is (= :none (:authority description)))))

(deftest closure-does-not-become-certificate-or-runtime-permission
  (let [g (fresh-graph)
        h (checked g)
        description (deps/describe-induction-dependencies h)]
    (is (not (certificate/checked-certificate? h)))
    (is (not (admission/admitted? h)))
    (doseq [k [:runtime-authorized :proved? :admitted? :valid?
               :physical-writers-closed :initializers-closed :tx-refined
               :proof-kernel-sound :proof-certificate]]
      (is (not (contains? description k))))))

(deftest incomplete-initial-case-cannot-gain-dependency-authority
  (let [bad (assoc-in catalog [:invariants :request/false]
                      {:readers #{} :expression [:literal false]})]
    (is (some? (data-from #(deps/build-induction-dependency-graph! bad))))
    (is (some? (data-from #(deps/check-induction-dependencies!
                            bad (fresh-graph)))))))
