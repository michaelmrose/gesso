(ns gesso.assurance.proof.induction-crosscheck-test
  "v1311: permanent original-catalog cross-check between Gesso's source-derived
   conditional induction and the separately implemented finite-model oracle.

   Each cross-check submits precisely the same ORIGINAL catalog to both
   implementations. Finite representative domains and the oracle's semantic
   restrictions are explicit additional hypotheses, not facts inferred from
   an induction certificate. A bounded oracle success does not prove a
   universal induction claim, and an open symbolic step does not establish a
   concrete counterexample. An actual finite counterexample must be treated
   as a serious disagreement if the symbolic claim says fully inductive.

   Neither report is an independent proof of either checker, an authenticated
   runtime trace, a physical XTDB correspondence theorem, or write authority."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.finite-model-oracle :as oracle]
            [gesso.assurance.proof.induction :as induction]))

(def ^:private types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/version {:type :integer}
   :request/owner {:type :keyword :optional? true :nullable? true}})
(def ^:private status-valid
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed :done}]]})
(def ^:private version-valid
  {:readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})
(def ^:private initial
  {:readers #{:request/status :request/version}
   :expression [:and
                (:expression status-valid)
                (:expression version-valid)]})

(defn- operation [id from to]
  {:id id
   :construction
   {:subject {:kind :operation :id id :interface {:operation id}}
    :state-types types :input-types {}
    :guard {:readers #{:request/status}
            :expression [:= [:read :request/status] [:literal from]]}
    :reads #{:request/status}
    :writes #{:request/status}
    :replacements {:request/status [:literal to]}
    :frame #{:request/version :request/owner}}})

(def ^:private catalog
  {:id :request/induction-crosscheck
   :declared-operation-ids #{:request/claim :request/unclaim}
   :initial-condition initial
   :invariants
   {:request/status-domain status-valid
    :request/version-nonnegative version-valid
    :request/truth {:readers #{} :expression [:literal true]}}
   :operations [(operation :request/claim :open :claimed)
                (operation :request/unclaim :claimed :open)]})

(def ^:private source
  {:catalog catalog
   :state-domains {:request/status [:open :claimed :done]
                   :request/version [-1 0 1]
                   :request/owner [oracle/absent nil :person/alice]}
   :input-domains {:request/claim {} :request/unclaim {}}
   :max-cases 1000})

(defn- crosscheck [s]
  {:symbolic (induction/check-induction! (:catalog s))
   :finite (oracle/check-finite-model! s)})
(defn- row [r id]
  (first (filter #(= id (:invariant %)) (:induction-outcomes r))))
(defn- finite [s] (oracle/check-finite-model! s))
(defn- set-invariant [s id expression readers]
  (assoc-in s [:catalog :invariants id]
            {:readers readers :expression expression}))
(defn- exception-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- open? [r id]
  (= :open (:status (row r id))))
(defn- strictly-conditional [r]
  (is (= induction/report-type (:type r)))
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r)))
  (is (= :not-independently-proved (:checker-soundness r)))
  (is (= :not-established (:physical-write-path-coverage r))))
(defn- strictly-diagnostic [r]
  (is (= oracle/report-type (:type r)))
  (is (= :none (:authority r)))
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :not-established (:universal-induction r)))
  (is (= :not-established (:checker-soundness r)))
  (is (= :not-established (:physical-correspondence r))))

(def ^:private sound (delay (crosscheck source)))

(deftest checked-induction-agrees-with-enumerated-branching-machine
  (let [{:keys [symbolic finite]} @sound]
    (strictly-conditional symbolic)
    (strictly-diagnostic finite)
    (is (:all-declared-invariants-conditionally-inductive? symbolic))
    (is (= (set (keys (:invariants catalog)))
           (:conditionally-checked-inductive-invariant-ids symbolic)))
    (is (= :bounded-no-counterexample (:status finite)) (pr-str finite))
    (is (:exhaustive? finite))
    (is (= 27 (get-in finite [:universe :states])))
    (is (= 81 (get-in finite [:universe :potential-cases])))
    (is (= 27 (get-in finite [:examined :initial-states-examined])))
    (is (= 18 (get-in finite [:examined :initial-states-satisfying])))
    (is (= 54 (get-in finite [:examined :transition-cases-examined])))
    (is (= 18 (get-in finite [:examined :enabled-transitions-examined])))
    (is (false? (get-in finite [:universe :all-type-domains-complete?])))))

(deftest all-original-invariants-have-every-preservation-operation
  (let [{:keys [symbolic finite]} @sound
        ids (:declared-operation-ids catalog)]
    (is (= ids (:declared-operation-ids symbolic)))
    (is (= (set (keys (:invariants catalog)))
           (set (map :invariant (:induction-outcomes symbolic)))))
    (doseq [outcome (:induction-outcomes symbolic)]
      (is (= :checked-conditional-induction (:status outcome)))
      (is (= :checked-conditional-inference (:initial-status outcome)))
      (is (= ids (:preserving-operation-ids outcome)))
      (is (empty? (:unsupported-operation-ids outcome))))
    (is (= (count ids) (count (get-in finite [:universe :input-counts-by-operation]))))
    (is (= (set (keys (:input-domains source))) ids))))

(deftest step-counterexample-challenges-open-preservation
  (let [s (-> (set-invariant source :request/always-open
                              [:= [:read :request/status] [:literal :open]]
                              #{:request/status})
              (assoc-in [:catalog :initial-condition]
                        {:readers #{:request/status :request/version}
                         :expression [:and
                                      [:= [:read :request/status] [:literal :open]]
                                      (:expression version-valid)]}))
        {:keys [symbolic finite]} (crosscheck s)
        witness (:counterexample finite)]
    (is (open? symbolic :request/always-open))
    (is (= :some-preservation-open (:reason (row symbolic :request/always-open))))
    (is (= :counterexample (:status finite)) (pr-str finite))
    (is (= :step (:kind witness)))
    (is (= :request/claim (:operation-id witness)))
    (is (= :request/always-open (:invariant-id witness)))
    (is (= :open (get-in witness [:prestate :request/status])))
    (is (= :claimed (get-in witness [:poststate :request/status])))
    (is (= {} (:input witness)))
    (is (= {:all-pre-invariants true :guard true} (:antecedent witness)))
    (is (= {:post-invariant false} (:consequent witness)))))

(deftest base-counterexample-challenges-open-initial-inference
  (let [s (set-invariant source :request/must-be-claimed
                         [:= [:read :request/status] [:literal :claimed]]
                         #{:request/status})
        {:keys [symbolic finite]} (crosscheck s)
        witness (:counterexample finite)]
    (is (open? symbolic :request/must-be-claimed))
    (is (= :initial-consequence-open
           (:reason (row symbolic :request/must-be-claimed))))
    (is (= :counterexample (:status finite)) (pr-str finite))
    (is (= :base (:kind witness)))
    (is (= :request/must-be-claimed (:invariant-id witness)))
    (is (= :open (get-in witness [:state :request/status])))
    (is (= {:initial-condition true} (:antecedent witness)))
    (is (= {:invariant false} (:consequent witness)))))

(deftest weak-initial-condition-reveals-negative-version-witness
  (let [s (assoc-in source [:catalog :initial-condition]
                    {:readers #{} :expression [:literal true]})
        {:keys [symbolic finite]} (crosscheck s)
        witness (:counterexample finite)]
    (is (open? symbolic :request/version-nonnegative))
    (is (= :initial-consequence-open
           (:reason (row symbolic :request/version-nonnegative))))
    (is (= :counterexample (:status finite)) (pr-str finite))
    (is (= :base (:kind witness)))
    (is (= :request/version-nonnegative (:invariant-id witness)))
    (is (= -1 (get-in witness [:state :request/version])))))

(deftest bounded-success-does-not-reverse-symbolic-inference
  (let [s (-> source
              (assoc-in [:catalog :initial-condition]
                        {:readers #{} :expression [:literal true]})
              (assoc-in [:state-domains :request/version] [0 1]))
        {:keys [symbolic finite]} (crosscheck s)]
    (is (open? symbolic :request/version-nonnegative))
    (is (= :bounded-no-counterexample (:status finite)) (pr-str finite))
    (is (:exhaustive? finite))
    (is (false? (get-in finite [:universe :all-type-domains-complete?])))
    (is (= 18 (get-in finite [:universe :states])))
    (is (= :not-established (:universal-induction finite)))
    (is (= :not-independently-proved (:checker-soundness symbolic)))))

(defn- unsafe-input-operation []
  (-> (operation :request/unclaim :claimed :open)
      (assoc-in [:construction :input-types]
                {:input/new-version {:type :integer}})
      (assoc-in [:construction :reads]
                #{:request/status :input/new-version})
      (assoc-in [:construction :writes]
                #{:request/status :request/version})
      (assoc-in [:construction :replacements]
                {:request/status [:literal :open]
                 :request/version [:read :input/new-version]})
      (assoc-in [:construction :frame] #{:request/owner})))

(deftest full-input-enumeration-exposes-version-violation
  (let [s (-> source
              (assoc-in [:catalog :operations 1] (unsafe-input-operation))
              (assoc-in [:input-domains :request/unclaim]
                        {:input/new-version [-1 0 1]}))
        {:keys [symbolic finite]} (crosscheck s)
        witness (:counterexample finite)]
    (is (open? symbolic :request/version-nonnegative))
    (is (= #{:request/unclaim}
           (:unsupported-operation-ids (row symbolic :request/version-nonnegative))))
    (is (= :counterexample (:status finite)) (pr-str finite))
    (is (= :step (:kind witness)))
    (is (= :request/unclaim (:operation-id witness)))
    (is (= :request/version-nonnegative (:invariant-id witness)))
    (is (= :claimed (get-in witness [:prestate :request/status])))
    (is (>= (get-in witness [:prestate :request/version]) 0))
    (is (= -1 (get-in witness [:input :input/new-version])))
    (is (= -1 (get-in witness [:poststate :request/version])))
    (is (= :open (get-in witness [:poststate :request/status])))))

(deftest sampled-input-can-hide-a-real-preservation-counterexample
  (let [s (-> source
              (assoc-in [:catalog :operations 1] (unsafe-input-operation))
              (assoc-in [:input-domains :request/unclaim]
                        {:input/new-version [0 1]}))
        {:keys [symbolic finite]} (crosscheck s)]
    (is (open? symbolic :request/version-nonnegative))
    (is (= :bounded-no-counterexample (:status finite)) (pr-str finite))
    (is (= 27 (get-in finite [:universe :states])))
    (is (= 108 (get-in finite [:universe :potential-cases])))
    (is (false? (get-in finite [:universe :all-type-domains-complete?])))))

(deftest budget-exhaustion-never-proves-a-symbolically-checked-catalog
  (let [s (assoc source :max-cases 80)
        r (finite s)]
    (is (:all-declared-invariants-conditionally-inductive? (:symbolic @sound)))
    (is (= :budget-exceeded (:status r)))
    (is (= :finite-case-budget (:reason r)))
    (is (false? (:exhaustive? r)))
    (strictly-diagnostic r)))

(deftest excluding-an-enabled-poststate-invalidates-finite-closure
  (let [s (assoc-in source [:state-domains :request/status] [:open :done])
        r (finite s)]
    (is (= :unsupported-language (:status r)) (pr-str r))
    (is (= :poststate-outside-declared-universe (:reason r)))
    (is (= :out-of-universe (get-in r [:counterexample :kind])))
    (is (= :claimed (get-in r [:counterexample :poststate :request/status])))
    (is (false? (:exhaustive? r)))))

(deftest no-initial-states-is-not-a-vacuous-induction-crosscheck
  (let [s (assoc-in source [:catalog :initial-condition]
                    {:readers #{} :expression [:literal false]})
        {:keys [symbolic finite]} (crosscheck s)]
    (is (empty? (:conditionally-checked-inductive-invariant-ids symbolic)))
    (is (= (set (keys (:invariants catalog)))
           (:unsupported-inductive-invariant-ids symbolic)))
    (is (= :unsupported-language (:status finite)) (pr-str finite))
    (is (= :no-initial-states-in-declared-universe (:reason finite)))
    (is (= 0 (get-in finite [:examined :initial-states-satisfying])))
    (is (false? (:exhaustive? finite)))))

(deftest operation-order-does-not-change-abstract-crosscheck
  (let [s (update-in source [:catalog :operations] #(vec (reverse %)))
        {:keys [symbolic finite]} (crosscheck s)]
    (is (= (:symbolic @sound) symbolic))
    (is (= (:finite @sound) finite))
    (is (= #{:request/claim :request/unclaim}
           (set (keys (get-in finite [:universe :input-counts-by-operation])))))))

(deftest mutation-can-retain-success-but-invalidate-source-identity
  (let [s (assoc-in source [:catalog :operations 0
                             :construction :replacements :request/status]
                    [:literal :done])
        {:keys [symbolic finite]} (crosscheck s)]
    (is (:all-declared-invariants-conditionally-inductive? symbolic))
    (is (= :bounded-no-counterexample (:status finite)) (pr-str finite))
    (is (not= (:fingerprint (:symbolic @sound)) (:fingerprint symbolic)))
    (is (= :none (:authority symbolic)))
    (is (= :none (:authority finite)))))

(deftest incomplete-declared-operation-inventory-fails-both-checkers
  (let [s (update-in source [:catalog :operations] pop)
        data (exception-data #(induction/check-induction! (:catalog s)))
        r (finite s)]
    (is (= :gesso.assurance.proof.operation-set/invalid-operation-set
           (:error/type data)))
    (is (= :operation-id-coverage-mismatch (:reason data)))
    (is (= :invalid-source (:status r)))
    (is (= :operation-coverage-mismatch (:reason r)))
    (strictly-diagnostic r)))

(deftest incomplete-finite-input-inventory-does-not-change-symbolic-proof
  (let [s (assoc source :input-domains {:request/claim {}})
        r (finite s)]
    (is (:all-declared-invariants-conditionally-inductive? (:symbolic @sound)))
    (is (= :invalid-source (:status r)))
    (is (= :wrong-keys (:reason r)))
    (is (false? (:exhaustive? r)))))

(deftest copied-symbolic-report-is-not-an-oracle-source
  (let [s (assoc source :symbolic-proof (:symbolic @sound))
        r (finite s)]
    (is (= :invalid-source (:status r)))
    (is (= :wrong-keys (:reason r)))
    (strictly-diagnostic r)))

(deftest oracle-does-not-consult-a-forged-symbolic-checker
  (let [expected (finite source)]
    (with-redefs [induction/check-induction!
                  (fn [_] {:all-declared-invariants-conditionally-inductive? true
                           :authority :fabricated-certification})]
      (is (= expected (finite source))))))

(deftest forged-report-fields-never-promote-an-induction-claim
  (let [{:keys [symbolic finite]} @sound
        fake (assoc symbolic :authority :transaction-authority
                             :checker-soundness :proved)]
    (is (= :transaction-authority (:authority fake)))
    (is (= :none (:authority (induction/check-induction! catalog))))
    (is (= :not-established (:universal-induction finite)))
    (is (= :not-established (:physical-correspondence finite)))
    (is (not (contains? finite :proof-handle)))))

(deftest repeat-counterexample-is-byte-for-byte-deterministic
  (let [s (-> (set-invariant source :request/always-open
                              [:= [:read :request/status] [:literal :open]]
                              #{:request/status})
              (assoc-in [:catalog :initial-condition]
                        {:readers #{:request/status :request/version}
                         :expression [:and
                                      [:= [:read :request/status] [:literal :open]]
                                      (:expression version-valid)]}))
        a (finite s)
        b (finite s)]
    (is (= :counterexample (:status a)))
    (is (= (:counterexample a) (:counterexample b)))
    (is (= (:examined a) (:examined b)))
    (is (= (:universe a) (:universe b)))))

(deftest four-operation-acceptance-catalog-crosschecks-original-source
  (require 'gesso.assurance.proof.induction-acceptance-test)
  (let [c (deref (ns-resolve 'gesso.assurance.proof.induction-acceptance-test
                             'catalog))
        s {:catalog c
           :state-domains {:request/status [:open :claimed :completed :cancelled]
                           :request/version [-1 0 1]
                           :request/priority [:low :normal :urgent]
                           :request/owner [nil :person/alice]}
           :input-domains (zipmap (:declared-operation-ids c) (repeat {}))
           :max-cases 2000}
        {:keys [symbolic finite]} (crosscheck s)]
    (is (:all-declared-invariants-conditionally-inductive? symbolic))
    (is (= (set (keys (:invariants c)))
           (:conditionally-checked-inductive-invariant-ids symbolic)))
    (is (= :bounded-no-counterexample (:status finite)) (pr-str finite))
    (is (= 72 (get-in finite [:universe :states])))
    (is (= 360 (get-in finite [:universe :potential-cases])))
    (is (= 72 (get-in finite [:examined :initial-states-examined])))
    (is (= 288 (get-in finite [:examined :transition-cases-examined])))
    (strictly-conditional symbolic)
    (strictly-diagnostic finite)))

(deftest narrowing-initial-equality-can-outstrip-structural-symbolic-proof
  ;; The separately validated finite semantics knows that :open belongs to
  ;; the enum, but the restricted symbolic initial-consequence rule need not
  ;; derive enum membership from a bare equality. This is NOT a soundness
  ;; counterexample; the symbolic result is conservative.
  (let [s (assoc-in source [:catalog :initial-condition]
                    {:readers #{:request/status :request/version}
                     :expression [:and
                                  [:= [:read :request/status] [:literal :open]]
                                  (:expression version-valid)]})
        {:keys [symbolic finite]} (crosscheck s)]
    (is (open? symbolic :request/status-domain))
    (is (= :initial-consequence-open
           (:reason (row symbolic :request/status-domain))))
    (is (= :bounded-no-counterexample (:status finite)) (pr-str finite))
    (is (:exhaustive? finite))
    (is (= 6 (get-in finite [:examined :initial-states-satisfying])))
    (is (= :none (:authority symbolic) (:authority finite)))))

(deftest complete-three-by-three-guard-and-update-mutation-matrix
  ;; Nine ORIGINAL source mutations; the independent interpreter does not
  ;; consume symbolic reports. Exhaustive finite agreement is checked for
  ;; every operation guard and every type-valid replacement in the enum.
  (doseq [from [:open :claimed :done]
          to [:open :claimed :done]]
    (testing (str "Guard " from " -> " to)
      (let [s (-> source
                  (assoc-in [:catalog :operations 0 :construction :guard]
                            {:readers #{:request/status}
                             :expression [:= [:read :request/status]
                                          [:literal from]]})
                  (assoc-in [:catalog :operations 0 :construction :replacements
                             :request/status] [:literal to]))
            {:keys [symbolic finite]} (crosscheck s)]
        (is (:all-declared-invariants-conditionally-inductive? symbolic))
        (is (= :bounded-no-counterexample (:status finite)) (pr-str finite))
        (is (= 27 (get-in finite [:universe :states])))
        (is (= 81 (get-in finite [:universe :potential-cases])))
        (is (= :none (:authority symbolic) (:authority finite)))))))

(deftest absent-and-present-owner-remain-framed-through-both-checkers
  (let [{:keys [symbolic finite]} @sound
        domains (get-in finite [:universe :state-domains :request/owner])]
    (is (= [oracle/absent nil :person/alice] domains))
    (is (= 27 (get-in finite [:universe :states])))
    (is (= :bounded-no-counterexample (:status finite)))
    (is (:all-declared-invariants-conditionally-inductive? symbolic))
    (is (every? #(= :checked-conditional-induction (:status %))
                (:induction-outcomes symbolic)))))

(deftest original-open-obligations-remain-after-bounded-agreement
  (let [{:keys [symbolic finite]} @sound]
    (is (= :not-established (:external-operation-set-completeness symbolic)))
    (is (= :not-established (:external-initialization-coverage symbolic)))
    (is (= :not-established (:implementation-correspondence symbolic)))
    (is (= (:declared-operation-ids catalog)
           (set (keys (:original-open-obligation-ids-by-operation symbolic)))))
    (is (every? seq (vals (:original-open-obligation-ids-by-operation symbolic))))
    (is (= :not-established (:universal-induction finite)))
    (is (= :not-established (:physical-correspondence finite)))
    (is (= :none (:authority symbolic) (:authority finite)))))
