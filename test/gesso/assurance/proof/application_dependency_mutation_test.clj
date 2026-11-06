(ns gesso.assurance.proof.application-dependency-mutation-test
  "v1441: adversarial source-relative application dependency closure.

   Checks that a generic, structurally valid DAG is NOT sufficient to close
   the exact declared application. Every test begins with the original source;
   no copied report or mutated graph may expand the scope of v1440's in-process
   structural closure into physical writer coverage or proof certification."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.proof.induction-dependency-test :as fixture]
            [gesso.assurance.subject :as subject]))

(def ^:private catalog
  (-> (var-get #'gesso.assurance.proof.induction-dependency-test/catalog)
      (assoc :invariants {:request/truth {:readers #{}
                                           :expression [:literal true]}})
      (assoc :initial-condition {:readers #{}
                                 :expression [:literal true]})))

(def ^:private error-type
  :gesso.assurance.proof.application-dependency/invalid-application-dependencies)
(def ^:private generic-type
  :gesso.assurance.dependency/invalid-dependency-graph)

;; Expensive proof-ledger recomputation is intentionally used by the public
;; checker for each attack; reuse only the immutable original fixture graph.
(def ^:private original (delay (app/build-application-dependency-graph! catalog)))
(def ^:private checked (delay (app/check-application-dependencies! catalog @original)))

(defn- thrown-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [f kind reason]
  (let [data (thrown-data f)]
    (is (= kind (:error/type data)) (str "Expected typed rejection: " (pr-str data)))
    (is (= reason (:reason data)) (str "Unexpected rejection: " (pr-str data)))))

(defn- at-role [graph role]
  (first (filter #(= role (get-in % [:subject :interface :role]))
                 (:nodes graph))))

(defn- at-id [graph id]
  (first (keep-indexed (fn [idx node]
                         (when (= id (:id node)) idx))
                       (:nodes graph))))

(defn- modify-node [graph id f]
  (update-in graph [:nodes (at-id graph id)] f))

(defn- fingerprint [raw]
  (:fingerprint (subject/describe-subject raw)))

(defn- remove-nodes [graph ids]
  (-> graph
      (update :nodes
              (fn [nodes]
                (mapv #(update % :depends-on
                               (fn [edges] (into [] (remove (comp ids :id)) edges)))
                      (remove (comp ids :id) nodes))))))

(defn- coherent-substitution [graph original changed]
  ;; Keep every structural edge fingerprint and the newly admitted subject
  ;; internally consistent. Generic DAG checking can then accept the forgery.
  (let [id (:id original)
        updated (modify-node graph id
                             #(assoc % :subject changed
                                       :subject-handle
                                       (admission/admit-subject! changed)))
        new-fingerprint (fingerprint changed)]
    (update updated :nodes
            (fn [nodes]
              (mapv (fn [node]
                      (update node :depends-on
                              (fn [edges]
                                (mapv (fn [e]
                                        (if (= id (:id e))
                                          (assoc e :fingerprint new-fingerprint)
                                          e)) edges))))
                    nodes)))))

(deftest whole-application-root-is-not-an-induction-only-root
  (let [g @original
        app-root (first (:roots g))
        original-induction-root (:id catalog)
        ;; Keep only the old induction DAG; it is a valid graph for that smaller
        ;; purpose, but it is NOT the declared application assurance graph.
        base-ids (->> (:nodes g)
                      (filter #(contains? #{:conditional-induction
                                             :operation-catalog
                                             :shared-initial-condition
                                             :declared-invariant
                                             :declared-abstract-operation}
                                          (get-in % [:subject :interface :role])))
                      (map :id) set)
        induction-only {:roots #{original-induction-root}
                        :nodes (filterv #(contains? (conj base-ids original-induction-root) (:id %)) (:nodes g))}]
    (is (not= original-induction-root app-root))
    (is (dependency/checked-graph? (dependency/check-graph! induction-only)))
    (reject! #(app/check-application-dependencies! catalog induction-only)
             error-type :root-scope-mismatch)))

(deftest pruned-valid-dag-does-not-close-the-original-operation-catalog
  (let [g @original
        original-op (first (filter #(and (= :declared-abstract-operation
                                            (get-in % [:subject :interface :role]))
                                        (= :request/unclaim
                                           (get-in % [:subject :interface :operation-id])))
                                  (:nodes g)))
        evidence-op (first (filter #(and (= :conditional-operation-evidence
                                            (get-in % [:subject :interface :role]))
                                        (= :request/unclaim
                                           (get-in % [:subject :interface :operation-id])))
                                  (:nodes g)))
        pruned (remove-nodes g #{(:id original-op) (:id evidence-op)})]
    (is (every? some? [original-op evidence-op]))
    (is (dependency/checked-graph? (dependency/check-graph! pruned)))
    (reject! #(app/check-application-dependencies! catalog pruned)
             error-type :source-dependency-coverage-mismatch)))

(deftest generic-graph-accepts-internally-coherent-substituted-premises
  (let [g @original
        original (at-role g :original-open-assumptions)
        forged (-> (:subject original)
                   (assoc-in [:interface :open-meta-obligations] #{})
                   (assoc-in [:interface :proof-status] :proved))
        mutant (coherent-substitution g original forged)]
    (is (dependency/checked-graph? (dependency/check-graph! mutant)))
    (reject! #(app/check-application-dependencies! catalog mutant)
             error-type :source-dependency-content-mismatch)))

(deftest missing-duplicates-and-cycles-reject
  (let [g @original
        app-root (first (:roots g))
        leaf (at-role g :shared-initial-condition)
        root-node (get-in g [:nodes (at-id g app-root)])
        without-open (remove-nodes g #{(:id (at-role g :original-open-assumptions))})
        duplicate-node (update g :nodes conj leaf)
        repeated-edge (modify-node g app-root
                                   #(update % :depends-on conj
                                            (first (:depends-on %))))
        cyclic (modify-node g (:id leaf)
                            #(update % :depends-on conj
                                     {:id app-root
                                      :fingerprint
                                      (fingerprint (:subject root-node))}))]
    (reject! #(app/check-application-dependencies! catalog without-open)
             error-type :source-dependency-coverage-mismatch)
    (reject! #(app/check-application-dependencies! catalog duplicate-node)
             error-type :duplicate-node)
    ;; Set-based exact comparison cannot mask duplicate edges: the underlying
    ;; checked dependency graph still forbids them.
    (reject! #(app/check-application-dependencies! catalog repeated-edge)
             generic-type :duplicate-dependency)
    (reject! #(dependency/check-graph! cyclic) generic-type :cycle)
    (reject! #(app/check-application-dependencies! catalog cyclic)
             error-type :source-dependency-content-mismatch)))

(deftest edge-drift-and-untrusted-subject-handles-reject
  (let [g @original
        id (first (:roots g))
        forged-fingerprint (modify-node g id
                                        #(assoc-in % [:depends-on 0 :fingerprint]
                                                   (str "sha256:" (apply str (repeat 64 "0")))))
        leaf (at-role g :shared-initial-condition)
        forged-admission (modify-node g (:id leaf)
                                      #(assoc % :subject-handle
                                              (admission/admit-subject!
                                               {:kind :rule
                                                :id :forgery/other
                                                :interface {:role :invented}})))]
    (reject! #(app/check-application-dependencies! catalog forged-fingerprint)
             error-type :source-dependency-content-mismatch)
    (reject! #(app/check-application-dependencies! catalog forged-admission)
             :gesso.assurance.admission/invalid-admission :wrong-subject)))

(deftest captured-source-closure-needs-the-current-source-and-graph
  (let [g @original
        h @checked
        fresh (app/build-application-dependency-graph! catalog)
        changed (assoc-in catalog [:operations 0 :construction :subject :interface :revision]
                          :second)
        changed-graph (app/build-application-dependency-graph! changed)
        stale-graph (update-in fresh [:nodes 0 :depends-on] (constantly []))]
    (is (identical? h (app/require-current-application-dependencies!
                       h catalog fresh)))
    (reject! #(app/require-current-application-dependencies!
              h changed changed-graph)
             error-type :stale-application-source)
    (reject! #(app/require-current-application-dependencies!
              h catalog stale-graph)
             error-type :source-dependency-content-mismatch)))

(deftest reports-and-constructed-objects-are-not-closure-handles
  (let [g @original
        h @checked
        report (app/describe-application-dependencies h)
        copied (edn/read-string (pr-str report))
        forged (assoc copied :proof-status :proved
                             :certificate :issued
                             :authority :application-certified
                             :assumption-discharge :complete
                             :installed-writer-coverage :established)
        fake (app/->ApplicationDependencyHandle)]
    (doseq [candidate [nil g report copied forged fake (dependency/check-graph! g)]]
      (is (false? (app/checked-application-dependencies? candidate)))
      (reject! #(app/describe-application-dependencies candidate)
               error-type :unrecognized-closure-handle)
      (reject! #(app/require-current-application-dependencies!
                candidate catalog g)
               error-type :unrecognized-closure-handle))
    (is (= :structural-source-dependency-closure-only (:proof-status report)))
    (is (= :not-established (:installed-writer-coverage report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:authority report)))
    (is (contains? (:open-meta-obligations report)
                   :physical-write-path-coverage))))

(deftest valid-narrower-declarations-never-prove-installed-writer-coverage
  (let [c (-> catalog
              (assoc :declared-operation-ids #{:request/claim})
              (update :operations
                      #(filterv (fn [entry] (= :request/claim (:id entry))) %)))
        graph (app/build-application-dependency-graph! c)
        h (app/check-application-dependencies! c graph)
        report (app/describe-application-dependencies h)]
    (is (= #{:request/claim} (:operation-ids report)))
    (is (= :closed-declared-abstract-machine-only (:scope report)))
    (is (= :not-established (:installed-writer-coverage report)))
    (is (= :not-performed (:assumption-discharge report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:authority report)))
    (reject! #(app/require-current-application-dependencies! h catalog @original)
             error-type :stale-application-source)))
