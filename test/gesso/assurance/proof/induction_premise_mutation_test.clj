(ns gesso.assurance.proof.induction-premise-mutation-test
  "v1437: adversarial source/premise completeness at BOTH induction boundaries.

   An incomplete declaration may be rejected for internal inconsistency, or
   accepted as a different, smaller abstract machine. Neither case can prove
   actual initializer/writer coverage. Even source changes that preserve every
   tested invariant must invalidate earlier source-relative dependency evidence.
   These tests exercise finite conditional semantics and local graph closure;
   they do not certify physical XTDB execution or general checker soundness."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.abstract-execution-induction :as abstract]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.induction-dependency :as dependency]
            [gesso.assurance.proof.induction-ledger :as ledger]
            [gesso.assurance.proof.literal-assignment-soundness-test :as lit]
            [gesso.assurance.proof.preservation-composition-soundness-test :as composition]))

;; The first model has an independently enumerated three-state initializer
;; universe. An application may NOT infer physical completeness from any subset
;; it chooses to declare, even if that subset passes abstract induction.
(def ^:private initial-good lit/after)
(def ^:private initial-other (assoc lit/after :request/version 2))
(def ^:private initial-bad (assoc lit/after :request/version -1))
(def ^:private expected-initial-universe
  #{initial-good initial-other initial-bad})
(def ^:private expected-operation-universe #{:request/claim :request/reclaim})
(def ^:private abstract-invariant composition/frame-and-literal)

(defn- a-operation [id]
  {:id id
   :declaration
   (-> lit/transition
       (assoc-in [:subject :id] id)
       (assoc-in [:subject :interface :operation] id)
       (assoc-in [:guard :expression]
                 [:= [:read :request/status] [:literal :claimed]]))})

(def ^:private abstract-catalog
  {:id :request/induction-premise-attack
   :state-types (:state-types lit/transition)
   :invariant abstract-invariant
   :declared-initial-states [initial-good initial-other initial-bad]
   :declared-operation-ids expected-operation-universe
   :operations (mapv a-operation (sort expected-operation-universe))})

(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- no-authority! [report]
  (is (= :none (:authority report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-issued (:certificate report))))

(deftest failing-source-initializer-cannot-be-forged-away
  (let [real (abstract/derive-induction-schema! abstract-catalog)
        forged (-> real
                   (assoc :status :conditional-declared-induction-schema
                          :conditional-claim
                          :every-finite-matching-declared-history-preserves-invariant
                          :failing-initial-indices #{}))
        real-again (abstract/derive-induction-schema! abstract-catalog)
        fake-source (assoc abstract-catalog :initial-cases
                           (:initial-cases forged))]
    (is (= :open-base-or-step (:status real)))
    (is (= #{2} (:failing-initial-indices real)))
    (is (nil? (:conditional-claim real)))
    (is (not= forged real-again))
    (is (= real real-again))
    (is (= :wrong-keys (:reason (failure
                                #(abstract/derive-induction-schema! fake-source)))))
    (no-authority! real)
    (no-authority! forged)))

(deftest shrinking-the-declared-initial-universe-changes-the-claim
  (let [subset (assoc abstract-catalog :declared-initial-states
                      [initial-good initial-other])
        current (abstract/derive-induction-schema! subset)
        original (abstract/derive-induction-schema! abstract-catalog)
        omitted (set/difference expected-initial-universe
                                (set (:declared-initial-states subset)))
        witness (abstract/audit-finite-history! subset initial-bad [])]
    (is (= #{initial-bad} omitted))
    (is (= :conditional-declared-induction-schema (:status current)))
    (is (= :open-base-or-step (:status original)))
    (is (= subset (:source-catalog current)))
    (is (= :outside-declared-initial-set (:status witness)))
    (is (false? (:initial-in-declared-set? witness)))
    (is (= :not-established (:initializer-coverage current)))
    (is (contains? (:open-external-obligations current)
                   :actual-initializer-set-coverage))
    (no-authority! current)
    (no-authority! witness)))

(deftest missing-declared-operation-must-not-disappear-silently
  (doseq [[label mutation]
          [[:missing-entry (update abstract-catalog :operations pop)]
           [:missing-declared-id
            (update abstract-catalog :declared-operation-ids disj :request/reclaim)]
           [:invented-id
            (update abstract-catalog :declared-operation-ids conj :request/other)]
           [:duplicate-entry
            (update abstract-catalog :operations conj
                    (a-operation :request/claim))]]]
    (testing (name label)
      (let [result (failure #(abstract/derive-induction-schema! mutation))]
        (is (= :operation-set-coverage-mismatch (:reason result))
            (pr-str result))
        (is (:abstract-execution-induction/rejection result))))))

(deftest self-consistent-operation-subset-has-only-subset-assurance
  (let [subset (-> abstract-catalog
                   (update :declared-operation-ids disj :request/reclaim)
                   (update :operations #(vec (remove (fn [o]
                                                      (= :request/reclaim (:id o))) %)))
                   (assoc :declared-initial-states [initial-good]))
        report (abstract/derive-induction-schema! subset)
        missing (set/difference expected-operation-universe
                                (:declared-operation-ids report))]
    (is (= #{:request/reclaim} missing))
    (is (= :conditional-declared-induction-schema (:status report)))
    (is (= #{:request/claim} (:declared-operation-ids report)))
    (is (= #{:request/claim} (set (map :operation-id
                                     (:operation-cases report)))))
    (is (= :not-established (:physical-operation-completeness report)))
    (is (contains? (:open-external-obligations report)
                   :actual-operation-set-completeness))
    (no-authority! report)))

(deftest removing-an-invariant-clause-is-a-different-proposition
  (let [reduced-invariant
        {:id (:id abstract-invariant)
         :readers #{:request/status}
         :expression [:= [:read :request/status] [:literal :claimed]]}
        altered (-> abstract-catalog
                    (assoc :invariant reduced-invariant)
                    (assoc :declared-initial-states [initial-good initial-bad]))
        report (abstract/derive-induction-schema! altered)
        original (abstract/derive-induction-schema! abstract-catalog)]
    ;; The bad version satisfies the weakened *status-only* invariant, not the
    ;; declared original conjunction. A verifier may accept the new rule, but
    ;; never return evidence under the old rule's meaning/source identity.
    (is (neg? (:request/version initial-bad)))
    (is (= :conditional-declared-induction-schema (:status report)))
    (is (= :open-base-or-step (:status original)))
    (is (= reduced-invariant (:invariant (:source-catalog report))))
    (is (not= (mapv :typed-invariant-fingerprint (:operation-cases original))
              (mapv :typed-invariant-fingerprint (:operation-cases report))))
    (is (= :not-established (:universal-implementation-soundness report)))
    (no-authority! report)))

;; The second model exercises actual in-process source-bound induction graph
;; closure and complete per-invariant obligation ledgers, not just copyable
;; finite-history diagnostics.
(def ^:private state-types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/version {:type :integer}
   :request/owner {:type :keyword :nullable? true}})
(def ^:private domain
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed :done}]]})
(def ^:private nonnegative
  {:readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})

(defn- high-operation [id from to]
  {:id id
   :construction
   {:subject {:kind :operation :id id :interface {:operation id}}
    :state-types state-types
    :input-types {}
    :guard {:readers #{:request/status}
            :expression [:= [:read :request/status] [:literal from]]}
    :reads #{:request/status}
    :writes #{:request/status}
    :replacements {:request/status [:literal to]}
    :frame #{:request/version :request/owner}}})

(def ^:private high-catalog
  {:id :request/induction-stale-premise
   :declared-operation-ids #{:request/claim :request/unclaim}
   :initial-condition {:readers #{:request/status :request/version}
                       :expression [:and (:expression domain)
                                    (:expression nonnegative)]}
   :invariants {:request/domain domain
                :request/version-nonnegative nonnegative}
   :operations [(high-operation :request/claim :open :claimed)
                (high-operation :request/unclaim :claimed :open)]})

(defn- open-inference? [report]
  (and (false? (:all-declared-invariants-conditionally-inductive? report))
       (seq (:unsupported-inductive-invariant-ids report))))

(deftest stale-source-cannot-borrow-earlier-closed-dependency-handle
  (let [original (induction/check-induction! high-catalog)
        graph (dependency/build-induction-dependency-graph! high-catalog)
        handle (dependency/check-induction-dependencies! high-catalog graph)
        old-ledger (ledger/check-induction-ledger! high-catalog)
        mutants
        [[:omitted-operation (-> high-catalog
                                 (update :declared-operation-ids disj
                                         :request/unclaim)
                                 (update :operations pop))]
         [:omitted-invariant (update high-catalog :invariants dissoc
                                    :request/version-nonnegative)]
         [:strengthened-base (update-in high-catalog
                                       [:initial-condition :expression]
                                       (fn [expr] [:and expr [:literal true]]))]
         [:changed-guard (assoc-in high-catalog
                                   [:operations 0 :construction :guard :expression]
                                   [:= [:read :request/status] [:literal :done]])]]]
    (is (:all-declared-invariants-conditionally-inductive? original))
    (is (dependency/checked-induction-dependencies? handle))
    (is (identical? handle
                    (dependency/require-current-induction-dependencies!
                     handle high-catalog graph)))
    (is (= old-ledger
           (ledger/require-current-induction-ledger!
            old-ledger high-catalog)))
    (doseq [[label mutated] mutants]
      (testing (name label)
        (let [fresh (induction/check-induction! mutated)
              new-graph (dependency/build-induction-dependency-graph! mutated)
              closed (dependency/check-induction-dependencies! mutated new-graph)
              stale (failure #(dependency/require-current-induction-dependencies!
                               handle mutated new-graph))
              old-report (failure #(ledger/require-current-induction-ledger!
                                    old-ledger mutated))]
          ;; These are valid *other* machines; green on their own is not
          ;; permission to reuse evidence for the original declaration.
          (is (:all-declared-invariants-conditionally-inductive? fresh))
          (is (not= (:fingerprint original) (:fingerprint fresh)))
          (is (dependency/checked-induction-dependencies? closed))
          (is (= :stale-source-dependency-closure (:reason stale))
              (pr-str stale))
          (is (= :stale-or-forged-induction-ledger (:reason old-report))
              (pr-str old-report))
          (is (= :none (:authority
                        (dependency/describe-induction-dependencies closed))))
          (is (= :all-original-obligations-remain-open
                 (:original-obligation-status
                  (ledger/check-induction-ledger! mutated)))))))))

(deftest forged-or-weakened-initial-premises-stay-open
  (let [too-weak (assoc high-catalog :initial-condition
                        {:readers #{} :expression [:literal true]})
        empty-premise (assoc high-catalog :initial-condition
                             {:readers #{} :expression [:literal false]})
        injected (assoc high-catalog :proof-status :proved)]
    (is (open-inference? (induction/check-induction! too-weak)))
    (is (open-inference? (induction/check-induction! empty-premise)))
    (is (= :wrong-keys (:reason (failure #(induction/check-induction! injected)))))
    (no-authority! (ledger/check-induction-ledger! too-weak))))
