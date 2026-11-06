(ns gesso.assurance.proof.operation-interface-provenance-mutation-test
  "v1445: permanent adversarial regression for source-bound composition provenance.

   A manifest is a copyable description, not an admission credential. It may
   survive data serialization, but altered content, dependency lineage, open
   premises, or authority claims must not survive a source-relative recheck.
   Neither this suite nor a valid manifest proves application writer coverage,
   proof-kernel soundness, or real XTDB/browser execution refinement."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.proof.induction-dependency-test :as base]
            [gesso.assurance.proof.operation-interface-composition :as composition]
            [gesso.assurance.proof.operation-interface-provenance :as provenance]
            [gesso.assurance.subject :as subject]))

(def ^:private catalog
  (-> (var-get #'base/catalog)
      (assoc :invariants {:request/truth {:readers #{}
                                          :expression [:literal true]}})
      (assoc :initial-condition {:readers #{}
                                 :expression [:literal true]})))

;; Derive the fixture from ORIGINAL construction, never a prebuilt proof report.
;; Source-relative rechecks still independently recompute the evidence.
(def ^:private source-graph
  (delay (app/build-application-dependency-graph! catalog)))
(def ^:private closure
  (delay (app/check-application-dependencies! catalog @source-graph)))
(def ^:private composed
  (delay (composition/compose-operation-interfaces!
          catalog @source-graph @closure)))
(def ^:private original
  (delay (provenance/describe-composition-provenance!
          @composed catalog @source-graph @closure)))

(def ^:private provenance-error
  :gesso.assurance.proof.operation-interface-provenance/invalid-operation-composition-provenance)

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects-as! [f expected-type expected-reason]
  (let [actual (error-data f)]
    (is (map? actual) (str "Expected typed rejection " expected-reason))
    (is (= expected-type (:error/type actual)) (pr-str actual))
    (is (= expected-reason (:reason actual)) (pr-str actual))))

(defn- recheck [report]
  (provenance/require-current-composition-provenance!
   report @composed catalog @source-graph @closure))

(defn- reject-manifest! [mutant]
  ;; Isolate the last, exact-report comparison boundary from the expensive
  ;; induction recomputation. Separate tests below exercise actual source,
  ;; graph, composition, and authenticated-closure revalidation end to end.
  (with-redefs [provenance/describe-composition-provenance!
                (fn [_composition _catalog _graph _closure] @original)]
    (rejects-as! #(recheck mutant)
                 provenance-error :stale-or-substituted-composition-provenance)))

(defn- manifest-index [report]
  (get-in report [:lineage :graph]))

(defn- fingerprint [raw]
  (:fingerprint (subject/describe-subject raw)))

(defn- role-node [role]
  (first (filter #(= role (get-in % [:subject :interface :role]))
                 (:nodes @source-graph))))

(deftest original-manifest-retains-entire-source-dependency-lineage
  (let [report @original
        lineage (:lineage report)
        index (manifest-index report)
        nodes (:nodes @source-graph)
        sources (set (map :id nodes))
        actual-dependencies (set (for [{:keys [id depends-on]} nodes
                                       {:keys [fingerprint] dep-id :id} depends-on]
                                   [id dep-id fingerprint]))
        manifested-dependencies
        (set (for [[id {:keys [dependencies]}] (:nodes index)
                   {dep-id :id digest :fingerprint} dependencies]
               [id dep-id digest]))]
    (is (= provenance/provenance-type (:type report)))
    (is (= provenance/provenance-version (:version report)))
    (is (= sources (set (keys (:nodes index)))))
    (is (= (:roots @source-graph) (:roots index)))
    (is (= (count sources) (:node-count index)))
    (is (= (count actual-dependencies) (:edge-count index)))
    (is (= actual-dependencies manifested-dependencies))
    (is (every? (fn [{:keys [id subject]}]
                  (= (fingerprint subject)
                     (get-in index [:nodes id :subject-fingerprint])))
                nodes))
    (is (= (set (keys (:operations lineage))) (:operation-ids @composed)))
    (is (= (set (keys (:invariants lineage))) (:invariant-ids @composed)))
    (is (= (:fingerprint @composed) (:composition-fingerprint lineage)))
    (is (= (:open-meta-obligations @composed) (:open-meta-obligations lineage)))
    (is (= (:open-assumptions @composed) (:open-assumptions lineage)))
    (is (= (:original-open-obligation-ids-by-operation @composed)
           (:original-open-obligation-ids-by-operation lineage)))
    (doseq [role [:operation-catalog :shared-initial-condition
                  :source-derived-induction-ledger :original-open-assumptions
                  :declared-application-assurance]]
      (is (some? (role-node role)) (str "Missing source role " role)))
    (is (= :closed-declared-abstract-machine-only (:scope report)))
    (is (= :conditional-structural-evidence (:proof-status report)))
    (is (= :not-established (:installed-writer-coverage report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :not-performed (:assumption-discharge report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:authority report)))))

(deftest serializable-report-is-not-a-certificate-or-local-closure
  (let [r @original
        copied (edn/read-string (pr-str r))]
    (is (= r copied))
    (let [verified (recheck copied)]
      (is (= r verified))
      (is (= :not-issued (:certificate verified)))
      (is (= :none (:authority verified))))
    (is (false? (app/checked-application-dependencies? r)))
    (is (false? (app/checked-application-dependencies? copied)))
    (rejects-as! #(app/describe-application-dependencies copied)
                 :gesso.assurance.proof.application-dependency/invalid-application-dependencies
                 :unrecognized-closure-handle)
    (is (= :none (:authority copied)))))

(deftest authority-and-artifact-version-forgeries-reject
  (let [r @original
        mutated [(assoc r :certificate :issued)
                 (assoc r :authority :application-certified)
                 (assoc r :admission :admitted)
                 (assoc r :assumption-discharge :complete)
                 (assoc r :installed-writer-coverage :established)
                 (assoc r :implementation-correspondence :proved)
                 (assoc-in r [:lineage :versions :operation-interface] -1)
                 (assoc-in r [:lineage :versions :composition-provenance] 99)
                 (assoc r :fingerprint (str "sha256:" (apply str (repeat 64 "0"))))
                 (with-meta r {:certified true})]]
    (doseq [[idx m] (map-indexed vector mutated)]
      (testing (str "No copyable artifact can gain authority, mutation " idx)
        (reject-manifest! m)))))

(deftest incomplete-or-substituted-lineage-reject
  (let [r @original
        lineage (:lineage r)
        op-id (first (sort (keys (:operations lineage))))
        inv-id (first (sort (keys (:invariants lineage))))
        node-id (first (sort-by str (keys (get-in lineage [:graph :nodes]))))
        op-node-id (get-in lineage [:operations op-id :source-node-id])
        dependency-node-id (first (for [[id {:keys [dependencies]}]
                                        (get-in lineage [:graph :nodes])
                                        :when (seq dependencies)] id))
        mutated [(update-in r [:lineage :operations] dissoc op-id)
                 (update-in r [:lineage :invariants] dissoc inv-id)
                 (update-in r [:lineage :graph :nodes] dissoc node-id)
                 (assoc-in r [:lineage :operations op-id :source-fingerprint]
                           "sha256:forged")
                 (assoc-in r [:lineage :operations op-id :source-node-id]
                           :forgery/substituted-node)
                 (assoc-in r [:lineage :operations op-id :original-open-obligation-ids]
                           #{})
                 (assoc-in r [:lineage :graph :nodes op-node-id :subject-fingerprint]
                           "sha256:forged")
                 (update-in r [:lineage :graph :nodes dependency-node-id :dependencies]
                            subvec 1)
                 (update-in r [:lineage :open-meta-obligations]
                            disj :physical-write-path-coverage)
                 (assoc-in r [:lineage :invariants inv-id :induction-status]
                           :universally-proved)]]
    (is (contains? (:open-meta-obligations lineage) :physical-write-path-coverage))
    (is (seq (get-in lineage [:graph :nodes dependency-node-id :dependencies])))
    (doseq [[idx m] (map-indexed vector mutated)]
      (testing (str "Modified lineage must be rejected, mutation " idx)
        (reject-manifest! m)))))

(deftest stale-source-and-substituted-composition-reject-before-provenance
  (let [changed (assoc-in catalog
                          [:operations 0 :construction :subject :interface :revision]
                          :source-changed)
        replaced (assoc-in @composed
                           [:operation-contracts :request/claim :source-fingerprint]
                           "sha256:substituted")
        bad-graph (update-in @source-graph [:nodes 0 :depends-on]
                             (constantly []))]
    (rejects-as! #(provenance/describe-composition-provenance!
                   @composed changed @source-graph @closure)
                 :gesso.assurance.proof.application-dependency/invalid-application-dependencies
                 :stale-application-source)
    (rejects-as! #(provenance/describe-composition-provenance!
                   replaced catalog @source-graph @closure)
                 :gesso.assurance.proof.operation-interface-composition/invalid-operation-interface-composition
                 :stale-or-substituted-operation-composition)
    (rejects-as! #(provenance/describe-composition-provenance!
                   @composed catalog bad-graph @closure)
                 :gesso.assurance.proof.application-dependency/invalid-application-dependencies
                 :source-dependency-content-mismatch)))

(deftest forged-local-closure-cannot-bootstrap-provenance
  (let [report (app/describe-application-dependencies @closure)
        fake (app/->ApplicationDependencyHandle)]
    (doseq [candidate [report (edn/read-string (pr-str report)) fake]]
      (rejects-as! #(provenance/describe-composition-provenance!
                     @composed catalog @source-graph candidate)
                   :gesso.assurance.proof.application-dependency/invalid-application-dependencies
                   :unrecognized-closure-handle))))
