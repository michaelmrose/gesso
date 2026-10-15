(ns gesso.model.formal-obligation
  "Independent, reusable safety obligations for the finite prepared-transaction
   machine in gesso.model.formal-transition.

   This namespace states what a valid generic transition trace MUST satisfy;
   it neither performs transitions nor calls tx-model/step, XTDB, or Recife.
   A model checker may evaluate holds? over EVERY reachable state of an
   admitted subject. Calling holds? directly on a few observed states does
   not make a proof, and successful bounded model checking does not establish
   unbounded refinement or correctness of the physical executor.

   These obligations are intentionally about the Gesso-owned serial atomic
   transaction subset, not authorization, application rules, or HumanHelp.
   The subject determines the selected finite states/plans; omitted writers
   and policy dependencies are not inferred by these predicates."
  (:require [gesso.model.command :as command]
            [gesso.model.formal-transition :as formal]))

(def assurance-scope
  {:subject :gesso.model.formal-obligation/prepared-serial-safety-v1
   :derived-from :gesso.model.formal-transition/prepared-serial-v1
   :obligations #{:action-accounting
                  :observation-chain
                  :rejected-is-atomic
                  :committed-write-set
                  :outcome-check-consistency}
   :assumed #{:complete-finite-selected-plans
              :serial-atomic-reference-model
              :admitted-command-target-and-after-data}
   :not-established #{:recife-or-tlc-check-has-run
                      :soundness-of-model-checker-exploration
                      :actual-xtdb-implementation-refinement
                      :application-policy-or-authorization
                      :network-or-browser-correspondence
                      :unbounded-correctness}})

(def obligation-ids
  [:action-accounting
   :observation-chain
   :rejected-is-atomic
   :committed-write-set
   :outcome-check-consistency])

(defn- require-subject! [subject]
  (when-not (and (= :gesso.model.formal-transition/prepared-serial-v1
                    (:kind subject))
                 (= formal/assurance-scope (:scope subject))
                 (vector? (:plans subject))
                 (<= 1 (count (:plans subject)) 6)
                 (= (count (:plans subject)) (count (:actions subject)))
                 (= (set (:actions subject))
                    (set (map-indexed (fn [i _] [::formal/attempt i])
                                      (:plans subject)))))
    (throw (ex-info "Expected an admitted Gesso prepared-transaction subject."
                    {:error/type ::invalid-subject})))
  subject)

(defn- state-parts [state]
  {:documents (::formal/documents state)
   :pending (::formal/pending state)
   :observations (::formal/observations state)})

(defn- action-accounting? [subject state]
  (let [{:keys [documents pending observations]} (state-parts state)
        size (count (:actions subject))
        indexes (set (range size))
        observed (mapv :plan-index observations)]
    (and (map? documents)
         (vector? pending)
         (vector? observations)
         (every? map? observations)
         (every? indexes pending)
         (every? indexes observed)
         (= (count pending) (count (distinct pending)))
         (= (count observed) (count (distinct observed)))
         (= pending (vec (sort pending)))
         (= indexes (into (set pending) observed))
         (= size (+ (count pending) (count observed))))))

(defn- observation-chain? [subject state]
  (let [{:keys [documents observations]} (state-parts state)
        root (::formal/documents (:initial-state subject))]
    (and (map? documents)
         (vector? observations)
         (every? (fn [{:keys [before after]}]
                   (and (map? before) (map? after))) observations)
         (or (empty? observations)
             (= root (:before (first observations))))
         (every? (fn [[prior following]]
                   (= (:after prior) (:before following)))
                 (partition 2 1 observations))
         (= documents (if (seq observations)
                        (:after (peek observations))
                        root)))))

(defn- rejected-is-atomic? [state]
  (every? (fn [{:keys [status before after]}]
            (or (not= :rejected status) (= before after)))
          (::formal/observations state)))

(defn- committed-write-set? [subject state]
  (every?
   (fn [{:keys [status before after plan-index]}]
     (if (= :committed status)
       ;; Independently assert the exact materialized write set. Do not use
       ;; tx-model/step here: the assertion must detect a broken step result.
       (= after
          (reduce (fn [documents model-command]
                    (assoc documents (command/target model-command)
                           (command/after model-command)))
                  before
                  (:commands (nth (:plans subject) plan-index))))
       true))
   (::formal/observations state)))

(defn- outcome-check-consistency? [state]
  (every?
   (fn [{:keys [status checks failed-precondition]}]
     (let [failed (first (remove :satisfied? checks))]
       (and (vector? checks)
            (every? (fn [check] (and (map? check)
                                     (contains? check :satisfied?)
                                     (boolean? (:satisfied? check))))
                    checks)
            (case status
              :committed (and (nil? failed)
                              (nil? failed-precondition))
              :rejected (and (some? failed)
                             (= failed failed-precondition))
              false))))
   (::formal/observations state)))

(defn holds?
  "A BOOLEAN predicate for ONE named obligation at ONE machine state.

   Intended use: a Recife/TLC invariant calls this for every reachable
   state of a fixed prepared-subject. Backend bookkeeping fields on state are
   ignored. This function itself does not explore any state space and makes
   no proof claim; state-space completeness remains the checker's obligation.
   Invalid obligation IDs and subjects fail closed."
  [subject obligation-id state]
  (require-subject! subject)
  (when-not (some #{obligation-id} obligation-ids)
    (throw (ex-info "Unknown Gesso formal obligation."
                    {:error/type ::unknown-obligation
                     :obligation-id obligation-id})))
  (if-not (and (map? state)
               (map? (::formal/documents state))
               (vector? (::formal/pending state))
               (vector? (::formal/observations state)))
    false
    (case obligation-id
      :action-accounting (action-accounting? subject state)
      :observation-chain (observation-chain? subject state)
      :rejected-is-atomic (rejected-is-atomic? state)
      :committed-write-set (and (action-accounting? subject state)
                                (committed-write-set? subject state))
      :outcome-check-consistency (outcome-check-consistency? state))))

(defn violated-obligations
  "Diagnostic list ONLY. A zero-length list for one state is not a proof.
   Use Recife/TLC to check these predicates over the reachable state space."
  [subject state]
  (into [] (remove #(holds? subject % state)) obligation-ids))
