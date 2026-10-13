(ns gesso.model.resolution-evidence
  "Finite, checkable *execution evidence* for the existing durable-command
   incorporation construction and the gesso.model.tx serial reference machine.

   A caller supplies prepared incorporation plans, a pre-execution document
   snapshot, and *independently collected* observations of XTDB transaction
   statuses and the post-execution documents at every relevant target. This
   checker requires the command-record write to be present in the plan, then
   tests whether ONE serialization of those prepared plans explains BOTH all
   transaction outcomes and the observed document state.

   It does not execute application policy, authenticate the observation source,
   read XTDB, or inspect Live settlement projectors. In particular, supplying
   forged observations can produce a perfectly consistent witness: it is not
   an authenticated certificate, a universal concurrency proof, or evidence
   that production HumanHelp guards are complete. Use this as a reusable
   finite-checking kernel for real-DB integration tests, preserving the
   observation provenance and the known model assumptions separately."
  (:require
   [clojure.set :as set]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]))

(def input-keys
  #{:initial-state :incorporations :observations})

(def observation-keys
  #{:statuses :state :inspected-targets})

(def incorporation-keys
  #{:plan :record :scope})

(def evidence-scope
  {:checker :gesso.model.resolution-evidence/serial-observation-v1
   :checked #{:canonical-durable-record-create-in-each-plan
              :complete-declared-target-inspection
              :whole-transaction-status-correspondence
              :persisted-state-correspondence
              :finite-serial-order-witness}
   :assumed #{:independent-authoritative-observation
              :consistent-post-execution-database-snapshot
              :prepared-plans-are-the-actual-submitted-plans
              :complete-initial-snapshot-for-decision-relevant-documents
              :gesso.model.tx-model/serial-reference-semantics
              :database-serial-commit-abstraction}
   :not-established #{:authenticated-observation-provenance
                      :real-database-execution-from-supplied-data
                      :policy-guard-completeness
                      :domain-invariant-correctness
                      :correctness-of-live-projectors
                      :durable-replay-and-recovery-liveness
                      :unbounded-concurrent-linearizability
                      :formal-refinement-proof}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-evidence
                   :error/kind kind
                   :error/details details})))

(defn- exact-map! [m allowed kind]
  (when-not (and (map? m) (= allowed (set (keys m))))
    (fail! kind "Expected a closed resolution-evidence map."
           {:expected allowed :received m}))
  m)

(defn- require-target [target]
  (when-not (and (vector? target)
                 (= 2 (count target))
                 (keyword? (first target))
                 (some? (second target)))
    (fail! :invalid-target
           "An inspected transaction target must be [entity-type document-id]."
           {:target target}))
  target)

(defn- require-incorporation [incorporation]
  (exact-map! incorporation incorporation-keys :invalid-incorporation)
  (when-not (= resolution/assurance-scope (:scope incorporation))
    (fail! :invalid-incorporation-scope
           "Expected the current canonical command-incorporation construction scope."
           {:scope (:scope incorporation)}))
  (let [record (resolution/require-record (:record incorporation))
        plan (model/admitted-plan (:plan incorporation))
        target [resolution/command-entity-type (:xt/id record)]
        domain-writes (filter #(= resolution/command-entity-type
                                  (first (command/target %)))
                              (:commands plan))]
    (when-not (and (= 1 (count domain-writes))
                   (command/create? (first domain-writes))
                   (= target (command/target (first domain-writes)))
                   (= record (command/after (first domain-writes))))
      (fail! :invalid-durable-incorporation
             "An incorporation must atomically create its exact durable command record once."
             {:target target}))
    plan))

(defn- targets-in-plan [plan]
  (set/union
   (into #{} (map command/target) (:commands plan))
   (into #{} (map command/guard-target)
         (tx/effective-guards (:commands plan) (:guards plan)))))

(defn- validate-input [input]
  (exact-map! input input-keys :invalid-input)
  (let [{:keys [initial-state incorporations observations]} input
        initial-state (model/require-state initial-state)]
    (when-not (and (vector? incorporations)
                   (<= 1 (count incorporations) 6))
      (fail! :invalid-incorporations
             "Finite checking requires one to six prepared incorporations."
             {:count (when (sequential? incorporations)
                       (count incorporations))}))
    (let [plans (mapv require-incorporation incorporations)
          tracked (reduce set/union
                          (set (keys initial-state))
                          (map targets-in-plan plans))]
      (exact-map! observations observation-keys :invalid-observation)
      (let [{:keys [statuses state inspected-targets]} observations]
        (model/require-state state)
        (when-not (and (vector? statuses)
                       (= (count plans) (count statuses))
                       (every? #{:committed :rejected} statuses))
          (fail! :invalid-statuses
                 "One physical :committed/:rejected XTDB outcome is required for each prepared plan; Live replay is not a transaction outcome."
                 {:statuses statuses :plans (count plans)}))
        (when-not (and (set? inspected-targets)
                       (every? (fn [target] (require-target target)) inspected-targets)
                       (= tracked inspected-targets)
                       (every? inspected-targets (keys state)))
          (fail! :incomplete-inspection
                 "Observations must explicitly cover every pre-existing, guarded and written target, including absent records."
                 {:expected-targets tracked
                  :inspected-targets inspected-targets
                  :unexpected-present-targets
                  (set/difference (set (keys state)) tracked)}))
        {:initial-state initial-state
         :plans plans
         :tracked-targets tracked
         :observed observations}))))

(defn- path-statuses [path n]
  (reduce (fn [statuses {:keys [plan-index status]}]
            (assoc statuses plan-index status))
          (vec (repeat n nil))
          (:trace path)))

(defn check-execution
  "Check the *given* physical transaction observations against all serial
   outcomes of the admitted prepared incorporations.

   Input shape:

     {:initial-state {[:requests request-id] document, ...}
      :incorporations [resolution/incorporation-plan-result, ...]
      :observations {:statuses [:committed :rejected, ...] ; input order
                     :state {target currently-stored-document, ...}
                     :inspected-targets #{all-inspected-targets}}}

   The caller must read EVERY inspected target after transactions settle,
   omitting absent documents from :state but retaining them in
   :inspected-targets. Target coverage includes initial documents, all guarded
   dependencies, all writes and every command-result create. Only observed
   physical commit statuses are admitted; a Live replay or unresolved response
   must first be resolved to a separate authoritative transaction outcome.

   Returns :consistent with all matching serial orders and one concrete trace,
   or :counterexample with a reason and the finite expected outcomes. The
   result is an inspectable witness relative to caller-supplied evidence,
   NOT a certificate or a basis for promoting application-preflight claims."
  [input]
  (let [{:keys [initial-state plans observed tracked-targets]}
        (validate-input input)
        exploration (model/explore-orders initial-state plans)
        expected (mapv (fn [path]
                         {:order (:order path)
                          :statuses (path-statuses path (count plans))
                          :state (:final-state path)
                          :trace (:trace path)})
                       (:paths exploration))
        observed-statuses (:statuses observed)
        observed-state (:state observed)
        status-matches (filterv #(= observed-statuses (:statuses %)) expected)
        matches (filterv #(= observed-state (:state %)) status-matches)
        consistent? (boolean (seq matches))]
    {:status (if consistent? :consistent :counterexample)
     :reason (cond
               consistent? :matched-serial-reference
               (empty? status-matches) :impossible-transaction-statuses
               :else :persisted-state-disagrees)
     :scope evidence-scope
     :reference-scope model/reference-scope
     :exploration (:exploration exploration)
     :inspected-targets tracked-targets
     :observed {:statuses observed-statuses :state observed-state}
     :matching-orders (mapv :order matches)
     :witness (when consistent? (select-keys (first matches) [:order :trace]))
     :expected (when-not consistent?
                 (mapv #(select-keys % [:order :statuses :state]) expected))}))
