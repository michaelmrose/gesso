(ns gesso.assurance.proof.conditional-induction-audit-test
  "v1436: independent finite-history audit of conditional initial/step
   induction. The test-local machine computes states and invariant truth
   without consulting the production transition observer, lemma checker, or
   the proof's reported conclusions. A missing initial state or one open
   operation must never be hidden by successful examples from other paths.

   This is bounded, source-declared ABSTRACT evidence, not proof of real
   initializer coverage, physical writer completeness, independent universal
   kernel soundness, or certified XTDB execution."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.abstract-execution-induction :as induction]
            [gesso.assurance.proof.literal-assignment-soundness-test :as fixture]
            [gesso.assurance.proof.preservation-composition-soundness-test :as composition]))

;; The immutable source below is an ordinary admitted abstract construction;
;; expected results are independently specified in the functions following it.
(def ^:private types (:state-types fixture/transition))
(def ^:private invariant composition/frame-and-literal)
(def ^:private initial-a fixture/after)
(def ^:private initial-b
  (assoc fixture/after :request/owner :user/b :request/version 8))
(def ^:private initial-c
  (-> fixture/after (dissoc :request/note) (assoc :request/version 0)))
(def ^:private initials [initial-a initial-b initial-c])
(def ^:private actors [:user/a :user/b])
(def ^:private op-ids [:request/claim :request/reclaim])

(defn- source-operation [id]
  {:id id
   :declaration
   (-> fixture/transition
       (assoc-in [:subject :id] id)
       (assoc-in [:subject :interface :operation] id)
       (assoc-in [:guard :expression]
                 [:= [:read :request/status] [:literal :claimed]]))})

(def ^:private catalog
  {:id :request/conditional-induction-audit
   :state-types types
   :invariant invariant
   :declared-initial-states initials
   :declared-operation-ids (set op-ids)
   :operations (mapv source-operation op-ids)})

;; Intentionally NOT a generic interpretation of Gesso's rule language:
;; these are an independently stated, tiny business model and invariant.
(defn- locally-invariant? [s]
  (and (= :claimed (:request/status s))
       (integer? (:request/version s))
       (<= 0 (:request/version s))))

(defn- locally-enabled? [s]
  (= :claimed (:request/status s)))

(defn- locally-apply [s id actor]
  (when-not (and (some #{id} op-ids) (locally-enabled? s))
    (throw (ex-info "Local transition not enabled" {:operation id :state s})))
  (assoc s
         :request/status :claimed
         :request/note "saved"
         :request/owner actor))

(defn- local-history [initial commands]
  (reductions (fn [s [id actor]] (locally-apply s id actor))
              initial commands))

(def ^:private commands
  (vec (for [id op-ids actor actors] [id actor])))

(defn- all-commands [depth]
  (if (zero? depth)
    [[]]
    (for [prefix (all-commands (dec depth)) command commands]
      (conj (vec prefix) command))))

(def ^:private histories
  ;; 1 + 4 + 16 + 64 histories per initial state, up to length three.
  (vec (mapcat all-commands (range 4))))

(defn- witness-steps [initial commands]
  (let [states (rest (local-history initial commands))]
    (mapv (fn [[id actor] after]
            {:operation-id id :inputs {:input/actor actor} :after after})
          commands states)))

(defn- exception-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- no-authority! [r]
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r))))

(deftest finite-independent-induction-over-every-declared-operation
  (let [report (induction/derive-induction-schema! catalog)]
    (is (= :conditional-declared-induction-schema (:status report)))
    (is (= #{:request/claim :request/reclaim}
           (set (map :operation-id (:operation-cases report)))))
    (is (= #{0 1 2} (set (map :index (:initial-cases report)))))
    (is (every? :invariant-holds? (:initial-cases report)))
    (is (empty? (:failing-initial-indices report)))
    (is (empty? (:unsupported-operation-ids report)))
    (is (= :every-finite-matching-declared-history-preserves-invariant
           (:conditional-claim report)))
    (no-authority! report))
  ;; Independent complete bounded history exploration: 3 * 85 = 255
  ;; histories, including all combinations of operation id and input actor.
  (is (= 85 (count histories)))
  (doseq [initial initials commands* histories]
    (let [states (vec (local-history initial commands*))]
      (is (= (inc (count commands*)) (count states)))
      (is (every? locally-invariant? states)
          (str "independent counterexample: " (pr-str commands*)))
      (doseq [[before [id actor] after]
              (map vector states commands* (rest states))]
        (is (locally-enabled? before))
        (is (= (locally-apply before id actor) after))
        (is (= (:request/version before) (:request/version after)))
        ;; A successful write fills optional note even if absent before.
        (is (contains? after :request/note))))))

(deftest independently-generated-histories-match-production-observation
  ;; The runtime audit rederives the complete schema for each witness. Cover
  ;; zero, one, two, three steps, both operations, both actors and all three
  ;; different initial states without turning this into an expensive full
  ;; application acceptance suite.
  (doseq [[i initial] (map-indexed vector initials)
          commands* (concat [[]]
                            (take 4 (all-commands 1))
                            (take 6 (drop (* i 3) (all-commands 2)))
                            (take 6 (drop (* i 7) (all-commands 3))))]
    (let [steps (witness-steps initial commands*)
          audit (induction/audit-finite-history! catalog initial steps)]
      (is (= :checked-finite-declared-history (:status audit)))
      (is (= :conditional-declared-induction-schema (:schema-status audit)))
      (is (= (count commands*) (:steps-matching audit)))
      (is (= (count commands*) (:steps-inspected audit)))
      (is (= (last (local-history initial commands*))
             (:last-matched-state audit)))
      (is (= (mapv first commands*)
             (mapv :operation-id (:step-observations audit))))
      (is (every? #(and (:before-invariant %)
                        (:after-invariant %)
                        (= :matching-abstract-step (:status %)))
                  (:step-observations audit)))
      (is (true? (:all-matched-states-satisfy-invariant? audit)))
      (is (= :finite-witness-not-universal-proof (:proof-status audit)))
      (no-authority! audit))))

(deftest bad-declared-base-opens-entire-induction
  (let [bad (assoc initial-b :request/version -1)
        c (update catalog :declared-initial-states conj bad)
        report (induction/derive-induction-schema! c)
        witness (induction/audit-finite-history! c bad [])]
    (is (false? (locally-invariant? bad)))
    (is (= :open-base-or-step (:status report)))
    (is (= #{3} (:failing-initial-indices report)))
    (is (empty? (:unsupported-operation-ids report)))
    (is (nil? (:conditional-claim report)))
    (is (= :finite-history-invariant-counterexample (:status witness)))
    (is (false? (:initial-invariant witness)))
    (no-authority! report)
    (no-authority! witness)))

(deftest one-nonpreserving-operation-opens-entire-induction
  (let [id :request/unclaim-unsafely
        unsafe (-> (source-operation id)
                   (assoc-in [:declaration :assignments :request/status]
                             [:literal :open]))
        c (-> catalog
              (update :declared-operation-ids conj id)
              (update :operations conj unsafe))
        report (induction/derive-induction-schema! c)
        broken (assoc (locally-apply initial-a :request/claim :user/a)
                      :request/status :open)
        witness (induction/audit-finite-history!
                 c initial-a [{:operation-id id
                               :inputs {:input/actor :user/a}
                               :after broken}])]
    (is (locally-invariant? initial-a))
    (is (not (locally-invariant? broken)))
    (is (= :open-base-or-step (:status report)))
    (is (= #{id} (:unsupported-operation-ids report)))
    (is (empty? (:failing-initial-indices report)))
    (is (nil? (:conditional-claim report)))
    (is (= :finite-history-invariant-counterexample (:status witness)))
    (is (= 1 (:steps-matching witness)))
    (is (false? (:all-matched-states-satisfy-invariant? witness)))
    (no-authority! report)
    (no-authority! witness)))

(deftest incomplete-declared-operation-catalog-rejects
  (doseq [c [(update catalog :declared-operation-ids disj :request/reclaim)
             (update catalog :declared-operation-ids conj :request/extra)
             (update catalog :operations pop)
             (update catalog :operations conj (source-operation :request/claim))]]
    (let [failure (exception-data #(induction/derive-induction-schema! c))]
      (is (= :operation-set-coverage-mismatch (:reason failure)))
      (is (:abstract-execution-induction/rejection failure)))))

(deftest open-physical-and-universal-premises-cannot-be-forgotten
  (let [report (induction/derive-induction-schema! catalog)]
    (is (= :conditional-schema-only (:proof-status report)))
    (is (= :not-established (:initializer-coverage report)))
    (is (= :not-established (:physical-operation-completeness report)))
    (is (= :not-established (:universal-implementation-soundness report)))
    (is (= :not-established (:xtdb-correspondence report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (every? (:open-external-obligations report)
                [:actual-initializer-set-coverage
                 :actual-operation-set-completeness
                 :physical-write-path-coverage
                 :universal-s1-s4-implementation-soundness
                 :xtdb-execution-correspondence
                 :certificate-admission]))
    (no-authority! report)))
